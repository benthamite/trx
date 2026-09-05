;;; trx-network-check.el --- Isolated socket acceptance checks -*- lexical-binding: t; -*-

;; Dynamic RPC bindings are visible to every timer in this Emacs process.
;; Refuse interactive loading before installing fixtures or requiring TRX.
(unless noninteractive
  (error "Run this fixture with make network-check in a clean batch Emacs"))

(require 'trx)
(require 'cl-lib)

(defun trx-network-check-one (mode)
  "Check nested socket refreshes in MODE using a disposable server."
  (let* ((target (generate-new-buffer " *trx-network-check*"))
         (trx-refresh-modes (list mode))
         (trx--refresh-in-progress nil)
         (trx--consecutive-failures 0)
         (trx-network-process-pool nil)
         (trx-host "127.0.0.1")
         (trx-rpc-auth nil)
         (trx-use-tls nil)
         (trx-daemon-auto-start nil)
         (trx-request-timeout 2)
         (requests 0) (ticks 0) (calls 0)
         timers clients response server)
    (unwind-protect
        (progn
          (setq server
                (make-network-process
                 :name "trx-network-check-server" :server t
                 :host "127.0.0.1" :service t :noquery t
                 :log (lambda (_server client _message) (push client clients))
                 :filter
                 (lambda (client text)
                   (process-put client :input
                                (concat (process-get client :input) text))
                   (when (string-match-p "\r\n\r\n" (process-get client :input))
                     (process-put client :input nil)
                     (cl-incf requests)
                     (when (= requests 1)
                       (push (run-at-time
                              0 nil
                              (lambda ()
                                (with-current-buffer target
                                  (cl-incf ticks)
                                  (trx-timer-revert))))
                             timers))
                     (push (run-at-time
                            0.05 nil
                            (lambda ()
                              (when (process-live-p client)
                                (let ((body "{\"result\":\"success\",\"arguments\":{\"ok\":true}}"))
                                  (process-send-string
                                   client
                                   (format "HTTP/1.1 200 OK\r\nContent-Length: %d\r\n\r\n%s"
                                           (string-bytes body) body))))))
                           timers)))))
          (let ((trx-service (process-contact server :service)))
            (with-current-buffer target
              ;; This isolates the mode gate and request lifecycle; it does
              ;; not exercise torrent rendering or any user buffer hooks.
              (setq major-mode mode)
              (setq-local revert-buffer-function
                          (lambda (&rest _)
                            (cl-incf calls)
                            (setq response (trx-request "torrent-get"))))
              (trx-timer-revert)
              (unless (and (= ticks 1) (= requests 1) (= calls 1)
                           (eq t (alist-get 'ok response))
                           (not trx--refresh-in-progress))
                (error "TRX network check nested refresh failed: %S"
                       (list mode ticks requests calls response)))
              ;; A second independent tick must refresh normally.
              (trx-timer-revert)
              (unless (and (= requests 2) (= calls 2)
                           (eq t (alist-get 'ok response))
                           (zerop trx--consecutive-failures))
                (error "TRX network check subsequent refresh failed: %S" mode))
              (list :mode mode :nested-ticks ticks :requests requests
                    :refreshes calls :response-ok t))))
      (mapc #'cancel-timer timers)
      (trx--flush-pool)
      (dolist (client clients)
        (when (process-live-p client) (delete-process client)))
      (when (and server (process-live-p server)) (delete-process server))
      (when (buffer-live-p target) (kill-buffer target)))))

(defun trx-network-check-interrupt ()
  "Check interrupted socket cleanup using a disposable server."
  (let* ((trx-network-process-pool nil)
         (trx-host "127.0.0.1")
         (trx-rpc-auth nil) (trx-use-tls nil)
         (trx-daemon-auto-start nil) (trx-request-timeout 2)
         clients process buffer outcome server
         (interrupt
          (lambda (candidate)
            (when (memq candidate trx-network-process-pool)
              (setq process candidate buffer (process-buffer candidate))
              (signal 'quit nil)))))
    (unwind-protect
        (progn
          (setq server
                (make-network-process
                 :name "trx-network-check-interrupt-server" :server t
                 :host "127.0.0.1" :service t :noquery t
                 :log (lambda (_server client _message) (push client clients))
                 :filter #'ignore))
          (let ((trx-service (process-contact server :service)))
            ;; Inject a nonlocal exit only into this fixture's connection,
            ;; after the actual HTTP send and before waiting for a response.
            (advice-add 'trx-wait :before interrupt)
            (setq outcome (condition-case nil
                              (trx-request "torrent-get")
                            (quit 'quit)))
            (unless (and (eq outcome 'quit) process
                         (not (process-live-p process))
                         (not (memq process trx-network-process-pool))
                         (not (buffer-live-p buffer)))
              (error "TRX network check interrupted connection was retained"))
            '(:interruption quit :socket-dead t :pool-empty t :buffer-dead t)))
      (advice-remove 'trx-wait interrupt)
      (trx--flush-pool)
      (dolist (client clients)
        (when (process-live-p client) (delete-process client)))
      (when (and server (process-live-p server)) (delete-process server)))))

(let ((results (mapcar #'trx-network-check-one
                       '(trx-mode trx-files-mode trx-info-mode trx-peers-mode))))
  (message "%S" (list :modes results
                       :interruption (trx-network-check-interrupt)))
  (when (cl-find-if (lambda (process)
                     (string-prefix-p "trx-network-check" (process-name process)))
                   (process-list))
    (error "Network check leaked fixture processes")))

;;; trx-network-check.el ends here
