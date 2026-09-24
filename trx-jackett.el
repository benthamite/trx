;;; trx-jackett.el --- Jackett search integration for trx -*- lexical-binding: t -*-

;; Copyright (C) 2026 Pablo Stafforini

;; Author: Pablo Stafforini <pablo@stafforini.com>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;;; Commentary:

;; Search torrent indexers via Jackett and add results to Transmission.
;; Requires a running Jackett instance.

;;; Code:

(require 'json)
(require 'xml)
(require 'trx)

(eval-when-compile
  (require 'cl-lib)
  (require 'let-alist)
  (require 'subr-x))

(defgroup trx-jackett nil
  "Jackett search integration for Trx."
  :group 'trx
  :link '(url-link "https://github.com/Jackett/Jackett"))

(defface trx-jackett-title
  '((t :inherit font-lock-keyword-face))
  "Face for torrent titles."
  :group 'trx-jackett)

(defface trx-jackett-tracker
  '((t :inherit font-lock-function-name-face))
  "Face for tracker names."
  :group 'trx-jackett)

(defface trx-jackett-category
  '((t :inherit font-lock-type-face))
  "Face for category descriptions."
  :group 'trx-jackett)

(defface trx-jackett-seeders
  '((t :inherit success))
  "Face for seeder counts."
  :group 'trx-jackett)

(defface trx-jackett-leechers
  '((t :inherit warning))
  "Face for leecher counts."
  :group 'trx-jackett)

(defface trx-jackett-size
  '((t :inherit shadow))
  "Face for size and age columns."
  :group 'trx-jackett)

(defcustom trx-jackett-host "localhost"
  "Host name or IP address of the Jackett instance."
  :type 'string)

(defcustom trx-jackett-port 9117
  "Port of the Jackett instance."
  :type 'integer)

(defcustom trx-jackett-api-key nil
  "API key for the Jackett instance.
If nil, looked up via `auth-source-search' using `trx-jackett-host'
and `trx-jackett-port'."
  :type '(choice (const :tag "Use auth-source" nil)
                 (string :tag "API key")))

(defcustom trx-jackett-categories nil
  "List of Torznab category IDs to filter search results.
Common categories: 2000 (Movies), 3000 (Audio), 5000 (TV),
7000 (Books).  When nil, all categories are searched."
  :type '(repeat integer))

(defcustom trx-jackett-use-tls nil
  "Whether to use HTTPS for the Jackett connection."
  :type 'boolean)

(defcustom trx-jackett-search-timeout 120
  "Maximum seconds to wait for each Jackett indexer or discovery request."
  :type 'natnum)

(defvar trx-jackett-search-history nil
  "History list for Jackett searches.")

(defvar-local trx-jackett--results nil
  "Vector of search result objects in the current buffer.")

(defvar-local trx-jackett--query nil
  "The search query that produced the current results.")

(defvar-local trx-jackett--pending nil
  "Names of indexers still being searched in the current buffer.")

(defvar-local trx-jackett--failures nil
  "Descriptions of failed indexers in the current search.")

(defun trx-jackett--api-key ()
  "Return the Jackett API key.
Tries, in order: `trx-jackett-api-key', Jackett's own
ServerConfig.json, and `auth-source'."
  (or trx-jackett-api-key
      (trx-jackett--api-key-from-config)
      (auth-source-pick-first-password
       :host trx-jackett-host
       :port trx-jackett-port)
      (user-error "No Jackett API key found")))

(defun trx-jackett--api-key-from-config ()
  "Read the API key from Jackett's ServerConfig.json."
  (let ((config (trx-jackett--config-file)))
    (when (and config (file-readable-p config))
      (with-temp-buffer
        (insert-file-contents config)
        (goto-char (point-min))
        (when (re-search-forward "\"APIKey\"\\s-*:\\s-*\"\\([^\"]+\\)\"" nil t)
          (match-string 1))))))

(defun trx-jackett--config-file ()
  "Return the path to Jackett's ServerConfig.json, or nil."
  (cl-find-if #'file-exists-p
              (list (expand-file-name
                     "~/Library/Application Support/Jackett/ServerConfig.json")
                    (expand-file-name "~/.config/Jackett/ServerConfig.json")
                    "/var/lib/jackett/ServerConfig.json")))

(defun trx-jackett--url (query)
  "Build the Jackett search URL for QUERY."
  (let ((scheme (if trx-jackett-use-tls "https" "http"))
        (params (list (cons "apikey" (url-hexify-string
                                        (trx-jackett--api-key)))
                      (cons "Query" (url-hexify-string query)))))
    (when trx-jackett-categories
      (push (cons "Category[]"
                  (mapconcat #'number-to-string
                             trx-jackett-categories ","))
            params))
    (format "%s://%s:%d/api/v2.0/indexers/all/results?%s"
            scheme trx-jackett-host trx-jackett-port
            (mapconcat (lambda (p) (concat (car p) "=" (cdr p)))
                       params "&"))))

(defun trx-jackett--format-size (bytes)
  "Format BYTES as a human-readable size string."
  (if (or (null bytes) (= 0 bytes)) "?"
    (file-size-human-readable bytes)))

(defun trx-jackett--format-age (date-string)
  "Format DATE-STRING as a relative age."
  (if (or (null date-string) (equal date-string ""))
      "?"
    (condition-case nil
        (let* ((time (date-to-time date-string))
               (secs (float-time (time-subtract nil time))))
          (trx-eta (abs secs) nil))
      (error "?"))))

;;;###autoload
(defun trx-jackett-search (query)
  "Search Jackett indexers for QUERY and display results."
  (interactive
   (list (read-string "Search Jackett: " nil 'trx-jackett-search-history)))
  (when (string-blank-p query)
    (user-error "Empty search query"))
  (message "Searching Jackett for \"%s\"..." query)
  (trx-jackett--fetch (trx-jackett--url query) query))

(defun trx-jackett--fetch (url query)
  "Discover indexers at URL and search each independently for QUERY."
  (let ((target (generate-new-buffer (format "*trx-search: %s*" query))))
    (with-current-buffer target
      (trx-jackett-results-mode)
      (setq trx-jackett--query query)
      (setq header-line-format (format "Searching Jackett for %S..." query)))
    (pop-to-buffer target)
    (trx-jackett--request
     (replace-regexp-in-string
      (regexp-quote "/results?")
      "/results/torznab/api?t=indexers&configured=true&" url t t)
     target
     (lambda (output failure)
       (if failure
           (trx-jackett--search-status
            target (concat "Cannot discover Jackett indexers: " failure))
         (condition-case nil
             (trx-jackett--search-indexers output url target)
           (error (trx-jackett--search-status
                   target "Cannot read Jackett indexer list"))))))))

(defun trx-jackett--request (url target callback)
  "Request URL for TARGET and call CALLBACK with output buffer and failure.
Exactly one argument to CALLBACK is non-nil.  Kill the output after it returns."
  (let ((output (generate-new-buffer " *trx-jackett*")))
    (condition-case nil
        (make-process
         :name "trx-jackett" :buffer output :connection-type 'pipe :noquery t
         :command (list "curl" "-s" "-f" "--connect-timeout" "10"
                        "--max-time" (number-to-string trx-jackett-search-timeout)
                        url)
         :sentinel
         (lambda (process _event)
           (when (memq (process-status process) '(exit signal))
             (unwind-protect
                 (when (buffer-live-p target)
                   (let* ((exit (process-exit-status process))
                          (failure
                           (cond ((eq (process-status process) 'signal)
                                  "request interrupted")
                                 ((= exit 28) "timed out")
                                 ((/= exit 0) (format "request failed (exit %d)" exit)))))
                     (funcall callback (unless failure output) failure)))
               (when (buffer-live-p output) (kill-buffer output))))))
      (error
       (kill-buffer output)
       (when (buffer-live-p target)
         (funcall callback nil "cannot start curl"))))))

(defun trx-jackett--search-indexers (output url target)
  "Read indexers from OUTPUT and start searches using URL in TARGET."
  (let* ((root (with-current-buffer output
                 (car (xml-parse-region (point-min) (point-max)))))
         (indexers (xml-get-children root 'indexer)))
    (unless (eq (xml-node-name root) 'indexers)
      (error "Invalid indexer list"))
    (dolist (indexer indexers)
      (unless (and (stringp (xml-get-attribute indexer 'id))
                   (not (string-empty-p (xml-get-attribute indexer 'id))))
        (error "Missing indexer ID")))
    (with-current-buffer target
      (setq trx-jackett--pending
            (mapcar (lambda (indexer) (xml-get-attribute indexer 'id)) indexers)))
    (if (null indexers)
        (trx-jackett--search-status target "No configured Jackett indexers")
      (trx-jackett--update-status target)
      (dolist (indexer indexers)
        (let ((id (xml-get-attribute indexer 'id)))
          (trx-jackett--request
           (replace-regexp-in-string
            "/indexers/all/" (concat "/indexers/" (url-hexify-string id) "/")
            url t t)
           target
           (lambda (buffer failure)
             (trx-jackett--finish-indexer buffer failure target id))))))))

(defun trx-jackett--finish-indexer (output failure target id)
  "Merge OUTPUT or record FAILURE for indexer ID in TARGET."
  (with-current-buffer target
    (setq trx-jackett--pending (delete id trx-jackett--pending))
    (unless failure
      (condition-case nil
          (let* ((json-object-type 'alist)
                 (json-array-type 'vector)
                 (json-key-type 'symbol)
                 (response (with-current-buffer output
                             (goto-char (point-min))
                             (json-read)))
                 (results (alist-get 'Results response))
                 (indexers (alist-get 'Indexers response)))
            (unless (and (vectorp results) (vectorp indexers))
              (error "Invalid Jackett response"))
            (when (seq-some (lambda (indexer)
                             (not (equal 2 (alist-get 'Status indexer))))
                           indexers)
              (setq failure "backend error"))
            (setq trx-jackett--results (vconcat trx-jackett--results results))
            (when (> (length results) 0)
              (revert-buffer)))
        (error (setq failure "cannot read or display results"))))
    (when failure
      (push (format "%s: %s" id failure) trx-jackett--failures))
    (trx-jackett--update-status target)))

(defun trx-jackett--update-status (target)
  "Show accumulated results, pending indexers and failures in TARGET."
  (with-current-buffer target
    (trx-jackett--search-status
     target
     (concat
      (format "%d results for %S" (length trx-jackett--results) trx-jackett--query)
      (if trx-jackett--pending
          (concat "; waiting for " (string-join trx-jackett--pending ", "))
        "; search complete")
      (when trx-jackett--failures
        (concat "; failed: " (string-join (reverse trx-jackett--failures) "; ")))))))

(defun trx-jackett--search-status (buffer status)
  "Show persistent STATUS in BUFFER and the echo area."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (tabulated-list-init-header)
      (setq header-line-format (list status "  |  " header-line-format))
      (force-mode-line-update)))
  (message "%s" status))

(defun trx-jackett--display-results (results query &optional target)
  "Display RESULTS from QUERY in TARGET or a new results buffer."
  (let ((buf (or target (get-buffer-create (format "*trx-search: %s*" query)))))
    (with-current-buffer buf
      (trx-jackett-results-mode)
      (setq trx-jackett--results results)
      (setq trx-jackett--query query)
      (revert-buffer)
      (goto-char (point-min)))
    (unless target (pop-to-buffer buf))
    (trx-jackett--search-status
     buf (if (zerop (length results))
             (format "No results for %S" query)
           (format "%d results for %S" (length results) query)))))

(defun trx-jackett--draw-results ()
  "Populate the results buffer from `trx-jackett--results'."
  (let (entries)
    (cl-loop for result across trx-jackett--results do
             (let-alist result
               (push (list result
                          (vector
                           (propertize (format "%d" (or .Seeders 0))
                                       'face 'trx-jackett-seeders)
                           (propertize (format "%d" (or .Peers 0))
                                       'face 'trx-jackett-leechers)
                           (propertize (trx-jackett--format-size .Size)
                                       'face 'trx-jackett-size)
                           (propertize (trx-jackett--format-age .PublishDate)
                                       'face 'trx-jackett-size)
                           (propertize (or .Tracker "?")
                                       'face 'trx-jackett-tracker)
                           (propertize (or .CategoryDesc "")
                                       'face 'trx-jackett-category)
                           (propertize (or .Title "")
                                       'face 'trx-jackett-title)))
                     entries)))
    (setq tabulated-list-entries (nreverse entries))
    (tabulated-list-print t)
    (trx--apply-fades)))

(defun trx-jackett-results-revert (_arg _noconfirm)
  "Revert function for the Jackett results buffer."
  (trx-jackett--draw-results))

(defun trx-jackett-add ()
  "Add the torrent at point to Transmission.
Uses the magnet URI when available; otherwise downloads the .torrent
file from the Jackett proxy link first.  When the result's category
matches an entry in `trx-category-directories', the torrent is added
to the configured directory."
  (interactive)
  (let ((result (tabulated-list-get-id)))
    (unless result
      (user-error "No result at point"))
    (let-alist result
      (let ((dir (trx-category-directory-for .CategoryDesc)))
        (cond
         ((and .MagnetUri (not (equal .MagnetUri :null)))
          (trx-add .MagnetUri dir (trx-indexer-labels .Tracker)))
         ((and .Link (not (equal .Link :null)))
          (trx-jackett--add-via-download .Link .Title dir .Tracker))
         (t (user-error "No magnet or download link")))))))

(defun trx-jackett--add-via-download (url title dir tracker)
  "Resolve URL and add the torrent to Transmission.
Jackett proxy links may redirect to a magnet URI or a .torrent file.
TITLE is used for status messages.  DIR, if non-nil, is the download
directory passed to `trx-add'.  TRACKER is the Jackett indexer name."
  (message "Resolving \"%s\"..." title)
  (let ((output ""))
    (set-process-sentinel
     (make-process
      :name "trx-jackett-resolve"
      :command (list "curl" "-s" "-o" "/dev/null"
                     "-w" "%{redirect_url}" url)
      :filter (lambda (_p s) (setq output (concat output s))))
     (lambda (process _event)
       (when (memq (process-status process) '(exit signal))
         (if (not (zerop (process-exit-status process)))
             (message "Failed to resolve torrent (exit %d)"
                      (process-exit-status process))
           (trx-jackett--add-resolved
            (string-trim output) url title dir tracker)))))))

(defun trx-jackett--add-resolved (redirect-url original-url title dir tracker)
  "Handle the resolved REDIRECT-URL from a Jackett proxy link.
If it is a magnet URI, pass it to `trx-add'.  If it is an HTTP URL,
download the .torrent file.  If empty, try ORIGINAL-URL directly.  TITLE
is used for status messages.  DIR, if non-nil, is the download directory
passed to `trx-add'.  TRACKER is the Jackett indexer name."
  (cond
   ((string-prefix-p "magnet:" redirect-url)
    (trx-add redirect-url dir (trx-indexer-labels tracker)))
   ((string-match-p "\\`https?://" redirect-url)
    (trx-jackett--download-torrent-file redirect-url title dir tracker))
   (t
    (trx-jackett--download-torrent-file original-url title dir tracker))))

(defun trx-jackett--download-torrent-file (url title dir tracker)
  "Download a .torrent file from URL and add it to Transmission.
TITLE is used for status messages.  DIR, if non-nil, is the download
directory passed to `trx-add'.  TRACKER is the Jackett indexer name."
  (let ((tmpfile (make-temp-file "trx-jackett-" nil ".torrent")))
    (message "Downloading \"%s\"..." title)
    (set-process-sentinel
     (start-process "trx-jackett-dl" nil
                    "curl" "-s" "-f" "-L" "-o" tmpfile url)
     (lambda (process _event)
       (if (not (zerop (process-exit-status process)))
           (progn
             (delete-file tmpfile t)
             (message "Failed to download torrent (exit %d)"
                      (process-exit-status process)))
         (trx-add tmpfile dir (trx-indexer-labels tracker))
         (run-at-time 5 nil #'delete-file tmpfile t))))))

(defun trx-jackett-browse-details ()
  "Open the details page for the result at point."
  (interactive)
  (let ((result (tabulated-list-get-id)))
    (unless result
      (user-error "No result at point"))
    (let ((url (cdr (assq 'Details result))))
      (if (and url (not (equal url :null)))
          (browse-url url)
        (user-error "No details URL available")))))

(defun trx-jackett-copy-magnet ()
  "Copy the magnet link for the result at point."
  (interactive)
  (let ((result (tabulated-list-get-id)))
    (unless result
      (user-error "No result at point"))
    (let ((magnet (cdr (assq 'MagnetUri result))))
      (if (and magnet (not (equal magnet :null)))
          (progn (kill-new magnet)
                 (message "Copied magnet link"))
        (user-error "No magnet link available")))))

(defun trx-jackett-search-again (query)
  "Run a new search from the results buffer."
  (interactive
   (list (read-string
          (format "Search Jackett [%s]: " trx-jackett--query)
          nil 'trx-jackett-search-history trx-jackett--query)))
  (trx-jackett-search query))

(define-trx-predicate jackett-seeders>? >
  (or (cdr (assq 'Seeders <>)) 0))

(define-trx-predicate jackett-peers>? >
  (or (cdr (assq 'Peers <>)) 0))

(define-trx-predicate jackett-size>? >
  (or (cdr (assq 'Size <>)) 0))

(defvar trx-jackett-results-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'trx-jackett-add)
    (define-key map "b" 'trx-jackett-browse-details)
    (define-key map "c" 'trx-jackett-copy-magnet)
    (define-key map "s" 'trx-jackett-search-again)
    map)
  "Keymap for `trx-jackett-results-mode'.")

(easy-menu-define trx-jackett-results-mode-menu trx-jackett-results-mode-map
  "Menu for `trx-jackett-results-mode'."
  '("Trx-Search"
    ["Add Torrent" trx-jackett-add]
    ["Browse Details" trx-jackett-browse-details]
    ["Copy Magnet Link" trx-jackett-copy-magnet]
    "--"
    ["New Search" trx-jackett-search-again]
    ["Quit" quit-window]))

(define-derived-mode trx-jackett-results-mode tabulated-list-mode
  "Trx-Search"
  "Major mode for viewing Jackett search results."
  :group 'trx-jackett
  (setq tabulated-list-format
        [("S" 4 trx-jackett-seeders>? :right-align t)
         ("L" 4 trx-jackett-peers>? :right-align t)
         ("Size" 7 trx-jackett-size>? :right-align t)
         ("Age" 5 t :right-align t)
         ("Tracker" 14 t)
         ("Cat" 10 t)
         ("Title" 0 t)])
  (setq tabulated-list-sort-key '("S"))
  (setq tabulated-list-printer #'trx-print-torrent)
  (tabulated-list-init-header)
  (setq-local revert-buffer-function #'trx-jackett-results-revert))

(provide 'trx-jackett)

;;; trx-jackett.el ends here
