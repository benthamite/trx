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

(require 'color)
(require 'json)
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

(defvar trx-jackett-search-history nil
  "History list for Jackett searches.")

(defvar-local trx-jackett--results nil
  "Vector of search result objects in the current buffer.")

(defvar-local trx-jackett--query nil
  "The search query that produced the current results.")

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
        (params (list (cons "apikey" (trx-jackett--api-key))
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
  "Fetch search results from URL for QUERY."
  (let ((buf (generate-new-buffer " *trx-jackett*")))
    (set-process-sentinel
     (start-process "trx-jackett" buf "curl" "-s" "-f" url)
     (lambda (process _event)
       (let ((buf (process-buffer process)))
         (if (not (zerop (process-exit-status process)))
             (progn
               (when (buffer-live-p buf) (kill-buffer buf))
               (message "Jackett search failed (exit %d)"
                        (process-exit-status process)))
           (when (buffer-live-p buf)
             (unwind-protect
                 (let* ((json-object-type 'alist)
                        (json-array-type 'vector)
                        (json-key-type 'symbol)
                        (response (with-current-buffer buf
                                    (goto-char (point-min))
                                    (json-read)))
                        (results (cdr (assq 'Results response))))
                   (if (or (null results) (zerop (length results)))
                       (message "No results for \"%s\"" query)
                     (trx-jackett--display-results results query)))
               (kill-buffer buf)))))))))

(defun trx-jackett--display-results (results query)
  "Display RESULTS from a search for QUERY in a results buffer."
  (let ((buf (get-buffer-create (format "*trx-search: %s*" query))))
    (with-current-buffer buf
      (trx-jackett-results-mode)
      (setq trx-jackett--results results)
      (setq trx-jackett--query query)
      (revert-buffer)
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (message "%d results for \"%s\"" (length results) query)))

(defvar-local trx-jackett--fade-overlays nil
  "Overlays for the truncation fade effect.")

(defun trx-jackett--truncate (string max-width &optional face)
  "Truncate STRING to MAX-WIDTH with optional FACE.
Mark the last three characters of truncated text with the
`trx-jackett-fade' property for post-rendering color blending."
  (let* ((truncated (> (string-width string) max-width))
         (result (if truncated
                     (truncate-string-to-width string max-width)
                   string)))
    (when face
      (setq result (propertize result 'face face))
      (when truncated
        (let ((len (length result)))
          (when (>= len 3)
            (dotimes (i 3)
              (put-text-property (+ (- len 3) i) (+ (- len 3) i 1)
                                'trx-jackett-fade (1+ i) result))))))
    result))

(defun trx-jackett--blend-color (fg bg ratio)
  "Blend FG toward BG by RATIO (0.0 = pure FG, 1.0 = pure BG)."
  (let ((fv (color-values fg))
        (bv (color-values bg)))
    (when (and fv bv)
      (format "#%02x%02x%02x"
              (ash (round (+ (* (- 1.0 ratio) (nth 0 fv))
                             (* ratio (nth 0 bv)))) -8)
              (ash (round (+ (* (- 1.0 ratio) (nth 1 fv))
                             (* ratio (nth 1 bv)))) -8)
              (ash (round (+ (* (- 1.0 ratio) (nth 2 fv))
                             (* ratio (nth 2 bv)))) -8)))))

(defun trx-jackett--resolve-foreground (face-val)
  "Resolve the effective foreground color from FACE-VAL."
  (cond
   ((symbolp face-val)
    (face-foreground face-val nil t))
   ((consp face-val)
    (cl-some (lambda (f)
               (and (facep f) (face-foreground f nil t)))
             face-val))))

(defun trx-jackett--apply-fades ()
  "Create fade overlays for characters marked with `trx-jackett-fade'."
  (mapc #'delete-overlay trx-jackett--fade-overlays)
  (setq trx-jackett--fade-overlays nil)
  (let ((bg (face-background 'default nil t)))
    (when bg
      (save-excursion
        (goto-char (point-min))
        (let ((pos (point-min)))
          (while (< pos (point-max))
            (let ((level (get-text-property pos 'trx-jackett-fade)))
              (if (not level)
                  (setq pos (or (next-single-property-change
                                 pos 'trx-jackett-fade nil (point-max))
                                (point-max)))
                (let* ((face-val (get-text-property pos 'face))
                       (fg (or (trx-jackett--resolve-foreground face-val)
                               (face-foreground 'default nil t)))
                       (ratio (* 0.25 level))
                       (blended (when fg
                                  (trx-jackett--blend-color fg bg ratio))))
                  (when blended
                    (let ((ov (make-overlay pos (1+ pos))))
                      (overlay-put ov 'face (list :foreground blended))
                      (overlay-put ov 'trx-jackett-fade t)
                      (push ov trx-jackett--fade-overlays))))
                (setq pos (1+ pos))))))))))

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
    (tabulated-list-print)
    (trx-jackett--apply-fades)))

(defun trx-jackett--print-entry (id cols)
  "Print a search result entry with fade truncation.
ID is the entry identifier, COLS is the column vector."
  (let ((beg (point))
        (x (max tabulated-list-padding 0))
        (ncols (length tabulated-list-format))
        (inhibit-read-only t))
    (when (> tabulated-list-padding 0)
      (insert (make-string x ?\s)))
    (dotimes (n ncols)
      (let* ((format (aref tabulated-list-format n))
             (width (nth 1 format))
             (props (nthcdr 3 format))
             (right-align (plist-get props :right-align))
             (pad-right (or (plist-get props :pad-right) 1))
             (label (aref cols n))
             (label-width (string-width label)))
        (cond
         ((= n (1- ncols))
          (insert (trx-jackett--truncate
                   label (- (window-width) x 1)
                   (get-text-property 0 'face label))))
         (right-align
          (let ((shift (- width label-width)))
            (when (> shift 0) (insert (make-string shift ?\s)))
            (insert label)
            (insert (make-string pad-right ?\s))
            (cl-incf x (+ width pad-right))))
         (t
          (insert (trx-jackett--truncate
                   label width
                   (get-text-property 0 'face label)))
          (let ((pad (- width (min label-width width))))
            (insert (make-string (+ pad pad-right) ?\s)))
          (cl-incf x (+ width pad-right))))))
    (insert ?\n)
    (put-text-property beg (point) 'tabulated-list-id id)))

(defun trx-jackett-results-revert (_arg _noconfirm)
  "Revert function for the Jackett results buffer."
  (trx-jackett--draw-results))

(defun trx-jackett-add ()
  "Add the torrent at point to Transmission.
Uses the magnet URI when available; otherwise downloads the
.torrent file from the Jackett proxy link first."
  (interactive)
  (let ((result (tabulated-list-get-id)))
    (unless result
      (user-error "No result at point"))
    (let-alist result
      (cond
       ((and .MagnetUri (not (equal .MagnetUri :null)))
        (trx-add .MagnetUri))
       ((and .Link (not (equal .Link :null)))
        (trx-jackett--add-via-download .Link .Title))
       (t (user-error "No magnet or download link"))))))

(defun trx-jackett--add-via-download (url title)
  "Resolve URL and add the torrent to Transmission.
Jackett proxy links may redirect to a magnet URI or a .torrent
file.  TITLE is used for status messages."
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
            (string-trim output) url title)))))))

(defun trx-jackett--add-resolved (redirect-url original-url title)
  "Handle the resolved REDIRECT-URL from a Jackett proxy link.
If it is a magnet URI, pass it to `trx-add'.  If it is an HTTP
URL, download the .torrent file.  If empty, try ORIGINAL-URL
directly.  TITLE is used for status messages."
  (cond
   ((string-prefix-p "magnet:" redirect-url)
    (trx-add redirect-url))
   ((string-match-p "\\`https?://" redirect-url)
    (trx-jackett--download-torrent-file redirect-url title))
   (t
    (trx-jackett--download-torrent-file original-url title))))

(defun trx-jackett--download-torrent-file (url title)
  "Download a .torrent file from URL and add it to Transmission.
TITLE is used for status messages."
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
         (trx-add tmpfile)
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
  (setq tabulated-list-sort-key '("S" . t))
  (setq tabulated-list-printer #'trx-jackett--print-entry)
  (tabulated-list-init-header)
  (setq-local revert-buffer-function #'trx-jackett-results-revert))

(provide 'trx-jackett)

;;; trx-jackett.el ends here
