;;; trx.el --- Interface to a Transmission session -*- lexical-binding: t -*-

;; Copyright (C) 2014-2021  Mark Oteiza <mvoteiza@udel.edu>

;; Author: Mark Oteiza <mvoteiza@udel.edu>
;; Version: 0.12.2
;; Package-Requires: ((emacs "24.4") (let-alist "1.0.5"))
;; Keywords: comm, tools

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Interface to a Transmission session.

;; Entry points are the `trx' and `trx-add'
;; commands.  A variety of commands are available for manipulating
;; torrents and their contents, many of which can be applied over
;; multiple items by selecting them with marks or within a region.
;; The menus for each context provide good exposure.

;; "M-x trx RET" pops up a torrent list.  One can add,
;; start/stop, verify, remove torrents, set speed limits, ratio
;; limits, bandwidth priorities, trackers, etc.  Also, one can
;; navigate to the corresponding file list, torrent info, or peer info
;; contexts.  In the file list, individual files can be toggled for
;; download, and their priorities set.

;; Customize-able are: the session address components, RPC
;; credentials, the display format of dates, file sizes and transfer
;; rates, pieces display, automatic refreshing of the torrent
;; list, etc.  See the `trx' customization group.

;; The design draws from a number of sources, including the command
;; line utility transmission-remote(1), the ncurses interface
;; transmission-remote-cli(1), and the rtorrent(1) client.  These can
;; be found respectively at the following:
;; <https://github.com/transmission/transmission/blob/master/utils/remote.c>
;; <https://github.com/fagga/transmission-remote-cli>
;; <https://rakshasa.github.io/rtorrent/>

;; Originally based on the JSON RPC library written by Christopher
;; Wellons, available online at <https://github.com/skeeto/elisp-json-rpc>.

;;; Code:

(require 'auth-source)
(require 'calc-bin)
(require 'calc-ext)
(require 'color)
(require 'diary-lib)
(require 'json)
(require 'mailcap)
(require 'tabulated-list)
(require 'url-util)

(eval-when-compile
  (cl-declaim (optimize (speed 3)))
  (require 'cl-lib)
  (require 'let-alist)
  (require 'subr-x))

(declare-function dired-goto-file "dired" (file))

(defgroup trx nil
  "Interface to a Transmission session."
  :link '(url-link "https://github.com/transmission/transmission")
  :link '(url-link "https://transmissionbt.com/")
  :group 'external)

(defface trx-torrent-name
  '((t :inherit font-lock-keyword-face))
  "Face for torrent names in the torrent list."
  :group 'trx)

(defface trx-torrent-size
  '((t :inherit shadow))
  "Face for size, ETA, ratio, and age columns."
  :group 'trx)

(defface trx-torrent-download
  '((t :inherit success))
  "Face for download rate column."
  :group 'trx)

(defface trx-torrent-upload
  '((t :inherit font-lock-constant-face))
  "Face for upload rate column."
  :group 'trx)

(defface trx-torrent-label
  '((t :inherit font-lock-type-face))
  "Face for torrent labels."
  :group 'trx)

(defface trx-file-name
  '((t :inherit font-lock-keyword-face))
  "Face for file names in the file list."
  :group 'trx)

(defface trx-file-priority
  '((t :inherit font-lock-function-name-face))
  "Face for file priority in the file list."
  :group 'trx)

(defface trx-peer-address
  '((t :inherit shadow))
  "Face for peer addresses."
  :group 'trx)

(defface trx-peer-client
  '((t :inherit font-lock-type-face))
  "Face for peer client names."
  :group 'trx)

(defface trx-peer-location
  '((t :inherit font-lock-function-name-face))
  "Face for peer locations."
  :group 'trx)

(defcustom trx-host "localhost"
  "Host name, IP address, or socket address of the Transmission session."
  :type 'string)

(defcustom trx-service 9091
  "Port or name of the service for the Transmission session."
  :type '(choice (const :tag "Default" 9091)
                 (string :tag "Service")
                 (integer :tag "Port"))
  :link '(function-link make-network-process))

(defcustom trx-rpc-path "/transmission/rpc"
  "Path to the Transmission session RPC interface."
  :type '(choice (const :tag "Default" "/transmission/rpc")
                 (string :tag "Other path")))

(defcustom trx-use-tls nil
  "Whether to use TLS for the RPC connection.
Requires Emacs to be compiled with GnuTLS support."
  :type 'boolean)

(defcustom trx-request-timeout 30
  "Timeout in seconds for RPC requests."
  :type 'number)

(defcustom trx-rpc-auth nil
  "Authentication (username, password, etc.) for the RPC interface.
Its value is a specification of the type used in `auth-source-search'.
If no password is set, `auth-sources' is searched using the
username, `trx-host', and `trx-service'."
  :type '(choice (const :tag "None" nil)
                 (plist :tag "Username/password"
                        :options ((:username string)
                                  (:password string))))
  :link '(info-link "(auth) Help for users")
  :link '(function-link auth-source-search))

(defcustom trx-digit-delimiter ","
  "String used to delimit digits in numbers.
The variable `calc-group-char' is bound to this in `trx-group-digits'."
  :type '(choice (const :tag "Comma" ",")
                 (const :tag "Full Stop" ".")
                 (const :tag "None" nil)
                 (string :tag "Other char"))
  :link '(variable-link calc-group-char)
  :link '(function-link trx-group-digits))

(defcustom trx-pieces-function #'trx-format-pieces
  "Function used to show pieces of incomplete torrents.
The function takes a string (bitfield) representing the torrent
pieces and the number of pieces as arguments, and should return a string."
  :type '(radio (const :tag "None" nil)
                (function-item trx-format-pieces)
                (function-item trx-format-pieces-brief)
                (function :tag "Function")))

(defcustom trx-trackers '()
  "List of tracker URLs.
These are used for completion in `trx-trackers-add' and
`trx-trackers-replace'."
  :type '(repeat (string :tag "URL")))

(defcustom trx-units nil
  "The flavor of units used to display file sizes.
See `file-size-human-readable'."
  :type '(choice (const :tag "Default" nil)
                 (const :tag "SI" si)
                 (const :tag "IEC" iec))
  :link '(function-link file-size-human-readable))

(defcustom trx-refresh-modes '()
  "List of major modes in which to refresh the buffer automatically."
  :type 'hook
  :options '(trx-mode
             trx-files-mode
             trx-info-mode
             trx-peers-mode))

(defcustom trx-refresh-interval 2
  "Period in seconds of the refresh timer."
  :type '(number :validate (lambda (w)
                             (when (<= (widget-value w) 0)
                               (widget-put w :error "Value must be positive")
                               w))))

(defcustom trx-time-format "%a %b %e %T %Y %z"
  "Format string used to display dates.
See `format-time-string'."
  :type 'string
  :link '(function-link format-time-string))

(defcustom trx-time-zone nil
  "Time zone of formatted dates.
See `format-time-string'."
  :type '(choice (const :tag "Local time" nil)
                 (const :tag "Universal Time (UTC)" t)
                 (const :tag "System Wall Clock" wall)
                 (string :tag "Time Zone Identifier"))
  :link '(info-link "(libc) TZ Variable")
  :link '(function-link format-time-string))

(defcustom trx-add-history-variable 'trx-add-history
  "History list to use for interactive prompts of `trx-add'.
Consider adding the value (`trx-add-history' by default)
to `savehist-additional-variables'."
  :type 'variable
  :link '(emacs-commentary-link "savehist"))

(defcustom trx-tracker-history-variable 'trx-tracker-history
  "History list to use for interactive prompts of tracker commands.
Consider adding the value (`trx-tracker-history' by default)
to `savehist-additional-variables'."
  :type 'variable
  :link '(emacs-commentary-link "savehist"))

(defcustom trx-torrent-functions
  '(trx-ffap trx-ffap-selection trx-ffap-last-killed)
  "List of functions to use for guessing torrents for `trx-add'.
Each function should accept no arguments, and return a string or nil."
  :type 'hook
  :options '(trx-ffap
             trx-ffap-selection
             trx-ffap-last-killed))

(defcustom trx-files-command-functions '(mailcap-file-default-commands)
  "List of functions to use for guessing default applications.
Each function should accept one argument, a list of file names,
and return a list of strings or nil."
  :type 'hook
  :options '(mailcap-file-default-commands))

(defcustom trx-geoip-function nil
  "Function used to translate an IP address into a location name.
The function should accept an IP address and return a string or nil."
  :type '(radio (const :tag "None" nil)
                (function-item trx-geoiplookup)
                (function :tag "Function")))

(defcustom trx-geoip-use-cache nil
  "Whether to cache IP address/location name associations.
If non-nil, associations are stored in `trx-geoip-table'.
Useful if `trx-geoip-function' does not have its own
caching built in or is otherwise slow."
  :type 'boolean)

(defcustom trx-turtle-lighter " turtle"
  "Lighter for `trx-turtle-mode'."
  :type '(choice (const :tag "Default" " turtle")
                 (const :tag "ASCII" " ,=,e")
                 (const :tag "Emoji" " \U0001f422")
                 (string :tag "Some string"))
  :set (lambda (symbol value)
         (set-default symbol value)
         (when (fboundp 'trx-turtle-poll) (trx-turtle-poll)))
  :link '(info-link "(elisp) Defining Minor Modes"))

(defcustom trx-daemon-program "transmission-daemon"
  "Program name for the Transmission daemon."
  :type 'string)

(defcustom trx-daemon-auto-start nil
  "Whether to auto-start the daemon when connection fails."
  :type 'boolean)

(defconst trx-schedules
  (eval-when-compile
    (pcase-let*
        ((`(,sun ,mon ,tues ,wed ,thurs ,fri ,sat)
          (cl-loop for x below 7 collect (ash 1 x)))
         (weekday (logior mon tues wed thurs fri))
         (weekend (logior sat sun))
         (all (logior weekday weekend)))
      `((sun . ,sun)
        (mon . ,mon)
        (tues . ,tues)
        (wed . ,wed)
        (thurs . ,thurs)
        (fri . ,fri)
        (sat . ,sat)
        (weekday . ,weekday)
        (weekend . ,weekend)
        (all . ,all))))
  "Alist of Trx turtle mode schedules.")

(defconst trx-mode-alist
  '((session . 0)
    (torrent . 1)
    (unlimited . 2))
  "Alist of threshold mode enumerations.")

(defconst trx-priority-alist
  '((low . -1)
    (normal . 0)
    (high . 1))
  "Alist of names to priority values.")

(defconst trx-status-names
  ["stopped"
   "verifywait"
   "verifying"
   "downwait"
   "downloading"
   "seedwait"
   "seeding"]
  "Array of possible Trx torrent statuses.")

(defconst trx-draw-torrents-keys
  ["hashString" "name" "status" "eta" "error" "labels"
   "rateDownload" "rateUpload"
   "percentDone" "sizeWhenDone" "metadataPercentComplete"
   "uploadRatio" "addedDate"])

(defconst trx-draw-files-keys
  ["name" "files" "downloadDir" "wanted" "priorities"])

(defconst trx-draw-info-keys
  ["id" "name" "hashString" "magnetLink" "labels" "activityDate" "addedDate"
   "dateCreated" "doneDate" "peers" "pieces" "pieceCount"
   "pieceSize" "trackerStats" "peersConnected" "peersGettingFromUs" "peersFrom"
   "peersSendingToUs" "sizeWhenDone" "error" "errorString" "uploadRatio"
   "downloadedEver" "corruptEver" "haveValid" "totalSize" "percentDone"
   "seedRatioLimit" "seedRatioMode" "bandwidthPriority" "downloadDir"
   "uploadLimit" "uploadLimited" "downloadLimit" "downloadLimited"
   "honorsSessionLimits" "rateDownload" "rateUpload" "queuePosition"])

(defconst trx-file-symbols
  '(:files-wanted :files-unwanted :priority-high :priority-low :priority-normal)
  "List of \"torrent-set\" method arguments for operating on files.")

(defvar trx-session-id nil
  "The \"X-Transmission-Session-Id\" header value.")

(defvar trx-add-history nil
  "Default history list for `trx-add'.")

(defvar trx-tracker-history nil
  "Default history list for `trx-trackers-add' and others.")

(defvar-local trx-torrent-vector nil
  "Vector of Trx torrent data.")

(defvar-local trx-torrent-id nil
  "The SHA-1 torrent info hash.")

(define-error 'trx-timeout "Trx request timed out")

(define-error 'trx-conflict
  "Wrong or missing header \"X-Transmission-Session-Id\"")

(define-error 'trx-unauthorized
  "Unauthorized user.  Check `trx-rpc-auth'")

(define-error 'trx-wrong-rpc-path
  "Bad RPC path.  Check `trx-rpc-path'")

(define-error 'trx-failure "RPC Failure")

(define-error 'trx-misdirected
  "Unrecognized hostname.  Check \"rpc-host-whitelist\"")

(defvar trx-timer nil
  "Timer for repeating `revert-buffer' in a visible Trx buffer.")

(defvar trx-geoip-table (make-hash-table :test 'equal)
  "Table for storing associations between IP addresses and location names.")

(defvar-local trx-marked-ids nil
  "List of identifiers of the currently marked items.")

(defvar trx-network-process-pool nil
  "List of network processes connected to Trx.")

(defvar trx-session-cache nil
  "Cached session data from the last `session-get' response.")

(defvar trx--consecutive-failures 0
  "Count of consecutive refresh failures.")

(defvar-local trx-filter-active nil
  "Active filter specification for the torrent list.
When non-nil, a string that torrent names must match.")

(defvar trx-filter-history nil
  "History list for `trx-filter'.")


;; JSON RPC

(defun trx--move-to-content ()
  "Move the point to beginning of content after the headers."
  (goto-char (point-min))
  (re-search-forward "^\r?\n" nil t))

(defun trx--content-finished-p ()
  "Return non-nil if all of the content has arrived."
  (goto-char (point-min))
  (when (search-forward "Content-Length: " nil t)
    (let ((length (read (current-buffer))))
      (and (trx--move-to-content)
           (<= length (- (position-bytes (point-max))
                         (position-bytes (point))))))))

(defun trx--status ()
  "Check the HTTP status code.
A 409 response from a Transmission session includes the
\"X-Transmission-Session-Id\" header.  If a 409 is received,
update `trx-session-id' and signal the error."
  (goto-char (point-min))
  (forward-char 5) ; skip "HTTP/"
  (skip-chars-forward "0-9.")
  (let* ((buffer (current-buffer))
         (status (read buffer)))
    (pcase status
      (200 (let (result)
             (when (and (trx--move-to-content)
                        (search-forward "\"result\":" nil t)
                        (not (equal "success" (setq result (json-read)))))
               (signal 'trx-failure (list result)))))
      ((or 301 404 405) (signal 'trx-wrong-rpc-path (list status)))
      (401 (signal 'trx-unauthorized (list status)))
      (403 (signal 'trx-failure (list status)))
      (409 (when (search-forward "X-Transmission-Session-Id: ")
             (setq trx-session-id (read buffer))
             (signal 'trx-conflict (list status))))
      (421 (signal 'trx-misdirected (list trx-host))))))

(defun trx--auth-source-secret (user)
  "Return the secret for USER at found in `auth-sources'.
Unless otherwise specified in `trx-rpc-auth', the host
and port default to `trx-host' and
`trx-service', respectively."
  (let ((spec (copy-sequence trx-rpc-auth)))
    (unless (plist-get spec :host) (plist-put spec :host trx-host))
    (unless (plist-get spec :port) (plist-put spec :port trx-service))
    (apply #'auth-source-pick-first-password (nconc `(:user ,user) spec))))

(defun trx--auth-string ()
  "HTTP \"Authorization\" header value if `trx-rpc-auth' is populated."
  (when trx-rpc-auth
    (let* ((user (plist-get trx-rpc-auth :username))
           (pass (and user (or (plist-get trx-rpc-auth :password)
                               (trx--auth-source-secret user)))))
      (concat "Basic " (base64-encode-string (concat user ":" pass) t)))))

(defun trx-http-post (process content)
  "Send to PROCESS an HTTP POST request containing CONTENT."
  (with-current-buffer (process-buffer process)
    (erase-buffer))
  (let ((headers (list (cons "X-Transmission-Session-Id" trx-session-id)
                       (cons "Host" trx-host) ; CVE-2018-5702
                       (cons "Content-length" (string-bytes content)))))
    (let ((auth (trx--auth-string)))
      (when auth (push (cons "Authorization" auth) headers)))
    (with-temp-buffer
      (insert (concat "POST " trx-rpc-path " HTTP/1.1\r\n"))
      (dolist (elt headers)
        (insert (format "%s: %s\r\n" (car elt) (cdr elt))))
      (insert "\r\n" content)
      (process-send-region process (point-min) (point-max)))))

(defun trx-wait (process)
  "Wait to receive HTTP response from PROCESS.
Return JSON object parsed from content.
Signals `trx-timeout' if `trx-request-timeout' is exceeded or
the connection dies."
  (with-current-buffer (process-buffer process)
    (let ((deadline (+ (float-time) trx-request-timeout)))
      (while (and (not (trx--content-finished-p))
                  (process-live-p process)
                  (< (float-time) deadline))
        (accept-process-output process 1))
      (unless (process-live-p process)
        (signal 'trx-timeout '("Connection closed by remote host")))
      (unless (trx--content-finished-p)
        (signal 'trx-timeout '("Request timed out"))))
    (trx--status)
    (trx--move-to-content)
    (when (search-forward "\"arguments\":" nil t)
      (json-read))))

(defun trx-send (process content)
  "Send PROCESS string CONTENT and wait for response synchronously."
  (trx-http-post process content)
  (trx-wait process))

(defun trx-process-sentinel (process _message)
  "Sentinel for PROCESS made by `trx-make-network-process'."
  (setq trx-network-process-pool
        (delq process trx-network-process-pool))
  (when (buffer-live-p (process-buffer process))
    (kill-buffer (process-buffer process))))

(defun trx-make-network-process ()
  "Return a network client process connected to a Transmission daemon.
When creating a new connection, the address is determined by the
custom variables `trx-host' and `trx-service'.
When `trx-use-tls' is non-nil, the connection uses TLS."
  (when (and trx-use-tls (not (gnutls-available-p)))
    (user-error "TLS requested but GnuTLS is not available"))
  (let ((socket (when (file-name-absolute-p trx-host)
                  (expand-file-name trx-host)))
        buffer process)
    (unwind-protect
        (condition-case err
            (prog1
                (setq buffer (generate-new-buffer " *trx*")
                      process
                      (make-network-process
                       :name "trx" :buffer buffer
                       :host (when (null socket) trx-host)
                       :service (or socket trx-service)
                       :family (when socket 'local)
                       :type (when (and trx-use-tls (null socket)) 'tls)
                       :noquery t :coding 'utf-8))
              (setq buffer nil process nil))
          (file-error
           (if (and trx-daemon-auto-start
                    (executable-find trx-daemon-program))
               (progn
                 (start-process "trx-daemon" nil trx-daemon-program)
                 (message "Started %s, retrying..." trx-daemon-program)
                 (sit-for 1)
                 (trx-make-network-process))
             (user-error "Cannot connect to Transmission at %s:%s -- %s"
                         trx-host trx-service (error-message-string err)))))
      (when (process-live-p process) (kill-process process))
      (when (buffer-live-p buffer) (kill-buffer buffer)))))

(defun trx-get-network-process ()
  "Return a network client process connected to a Trx daemon.
Returns a stopped process in `trx-network-process-pool'
or, if none is found, establishes a new connection and adds it to
the pool."
  (or (cl-loop for process in trx-network-process-pool
               when (process-command process) return (continue-process process))
      (let ((process (trx-make-network-process)))
        (push process trx-network-process-pool)
        process)))

(defun trx--flush-pool ()
  "Kill all processes in `trx-network-process-pool' and reset it."
  (dolist (process trx-network-process-pool)
    (when (process-live-p process) (kill-process process))
    (when (buffer-live-p (process-buffer process))
      (kill-buffer (process-buffer process))))
  (setq trx-network-process-pool nil))

(defun trx-request (method &optional arguments tag)
  "Send a request to Transmission and return a JSON object.
The JSON is the \"arguments\" object decoded from the response.
METHOD is a string.
ARGUMENTS is a plist having keys corresponding to METHOD.
TAG is an integer and ignored.
Retries once on transient failures.
Details regarding the Transmission RPC can be found here:
<https://github.com/transmission/transmission/blob/master/extras/rpc-spec.txt>"
  (let ((content (json-encode `(:method ,method :arguments ,arguments :tag ,tag)))
        (retries 1)
        result done)
    (while (not done)
      (let ((process (trx-get-network-process)))
        (set-process-plist process nil)
        (set-process-filter process nil)
        (set-process-sentinel process nil)
        (unwind-protect
            (condition-case err
                (progn
                  (setq result (trx-send process content))
                  (setq done t))
              (trx-conflict
               (setq result (trx-send process content))
               (setq done t))
              (trx-failure
               (message "%s" (cdr err))
               (setq done t))
              (trx-timeout
               (if (> retries 0)
                   (progn (cl-decf retries) (trx--flush-pool))
                 (message "Trx: %s" (cadr err))
                 (setq done t)))
              (file-error
               (if (> retries 0)
                   (progn (cl-decf retries) (trx--flush-pool))
                 (message "Trx: connection failed -- %s"
                          (error-message-string err))
                 (setq done t))))
          (when (and process (process-live-p process))
            (stop-process process))
          (when (and process (not (process-live-p process)))
            (setq trx-network-process-pool
                  (delq process trx-network-process-pool))
            (when (buffer-live-p (process-buffer process))
              (kill-buffer (process-buffer process)))))))
    result))


;; Asynchronous calls

(defun trx-process-callback (process)
  "Call PROCESS's callback if it has one."
  (let ((callback (process-get process :callback)))
    (when callback
      (trx--move-to-content)
      (when (search-forward "\"arguments\":" nil t)
        (run-at-time 0 nil callback (json-read))))))

(defun trx-process-filter (process text)
  "Handle PROCESS's output TEXT and trigger handlers."
  (internal-default-process-filter process text)
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (when (trx--content-finished-p)
        (condition-case e
            (progn (trx--status)
                   (trx-process-callback process)
                   (stop-process process))
          (trx-conflict
           (trx-http-post process (process-get process :request)))
          (trx-failure
           (message "%s" (cdr e)))
          (error
           (stop-process process)
           (signal (car e) (cdr e))))))))

(defun trx-request-async (callback method &optional arguments tag)
  "Send a request to Trx asynchronously.

CALLBACK accepts one argument, the response \"arguments\" JSON object.
METHOD, ARGUMENTS, and TAG are the same as in `trx-request'."
  (let ((process (trx-get-network-process))
        (content (json-encode `(:method ,method :arguments ,arguments :tag ,tag))))
    (set-process-filter process #'trx-process-filter)
    (set-process-sentinel process #'trx-process-sentinel)
    (process-put process :request content)
    (process-put process :callback callback)
    (trx-http-post process content)
    process))


;; Response destructuring

(defun trx-torrents (response)
  "Return the \"torrents\" array in RESPONSE, otherwise nil."
  (let ((torrents (cdr (assq 'torrents response))))
    (and (< 0 (length torrents)) torrents)))

(defun trx-unique-labels (torrents)
  "Return a list of unique labels from TORRENTS."
  (let (labels res)
    (dotimes (i (length torrents))
      (dotimes (j (length (setq labels (cdr (assq 'labels (aref torrents i))))))
        (cl-pushnew (aref labels j) res :test #'equal)))
    res))


;; Timer management

(defun trx-timer-revert ()
  "Revert the buffer or cancel `trx-timer'.
After 5 consecutive failures, cancel the timer."
  (if (and (memq major-mode trx-refresh-modes)
           (not (or (bound-and-true-p isearch-mode)
                    (buffer-narrowed-p)
                    (use-region-p))))
      (condition-case _err
          (progn
            (revert-buffer)
            (setq trx--consecutive-failures 0))
        (error
         (cl-incf trx--consecutive-failures)
         (when (>= trx--consecutive-failures 5)
           (cancel-timer trx-timer)
           (message "Trx: too many failures, auto-refresh disabled"))))
    (cancel-timer trx-timer)))

(defun trx-timer-run ()
  "Run the timer `trx-timer'."
  (when trx-timer (cancel-timer trx-timer))
  (setq
   trx-timer
   (run-at-time t trx-refresh-interval #'trx-timer-revert)))

(defun trx-timer-check ()
  "Check if current buffer should run a refresh timer."
  (when (memq major-mode trx-refresh-modes)
    (trx-timer-run)))


;; Other

(defun trx-refs (sequence key)
  "Return a list of the values of KEY in each element of SEQUENCE."
  (mapcar (lambda (x) (cdr (assq key x))) sequence))

(defun trx-size (bytes)
  "Return string showing size BYTES in human-readable form."
  (file-size-human-readable bytes trx-units))

(defun trx-percent (have total)
  "Return the percentage of HAVE by TOTAL."
  (if (zerop total) 0 (/ (* 100.0 have) total)))

(defun trx-slice (str k)
  "Slice STRING into K strings of somewhat equal size.
The result can have no more elements than STRING.
\n(fn STRING K)"
  (let ((len (length str)))
    (let ((quotient (/ len k))
          (remainder (% len k))
          (i 0)
          slice result)
      (while (and (/= 0 (setq len (length str))) (< i k))
        (setq slice (if (< i remainder) (1+ quotient) quotient))
        (push (substring str 0 (min slice len)) result)
        (setq str (substring str (min slice len) len))
        (cl-incf i))
      (nreverse result))))

(defun trx-text-property-all (beg end prop)
  "Return a list of non-nil values of a text property PROP between BEG and END.
If none are found, return nil."
  (let (res pos)
    (save-excursion
      (goto-char beg)
      (while (> end (point))
        (push (get-text-property (point) prop) res)
        (setq pos (text-property-not-all (point) end prop (car-safe res)))
        (goto-char (or pos end))))
    (nreverse (delq nil res))))

(defun trx-eta (seconds percent)
  "Return a string showing SECONDS in human-readable form;
otherwise some other estimate indicated by SECONDS and PERCENT."
  (if (<= seconds 0)
      (if (= percent 1) "Done"
        (if (char-displayable-p #x221e) "\u221e" "Inf"))
    (let* ((minute 60.0)
           (hour 3600.0)
           (day 86400.0)
           (month (* 29.53 day))
           (year (* 365.25 day)))
      (apply #'format "%.0f%s"
             (cond
              ((> minute seconds) (list seconds "s"))
              ((> hour seconds) (list (/ seconds minute) "m"))
              ((> day seconds) (list (/ seconds hour) "h"))
              ((> month seconds) (list (/ seconds day) "d"))
              ((> year seconds) (list (/ seconds month) "mo"))
              (t (list (/ seconds year) "y")))))))

(defun trx-when (seconds)
  "The `trx-eta' of time between `current-time' and SECONDS."
  (if (<= seconds 0) "never"
    (let ((secs (- seconds (float-time (current-time)))))
      (format (if (< secs 0) "%s ago" "in %s")
              (trx-eta (abs secs) nil)))))

(defun trx-rate (bytes)
  "Return a rate in units kilobytes per second.
The rate is calculated from BYTES according to `trx-units'."
  (/ bytes (if (eq 'iec trx-units) 1024 1000)))

(defun trx-throttle-torrent (ids limit n)
  "Set transfer speed limit for IDS.
LIMIT is a keyword; either :uploadLimit or :downloadLimit.
N is the desired threshold.  A non-positive value of N means to
disable the limit."
  (cl-assert (memq limit '(:uploadLimit :downloadLimit)))
  (let ((arguments `(:ids ,ids ,(pcase limit
                                  (:uploadLimit :uploadLimited)
                                  (:downloadLimit :downloadLimited))
                     ,@(if (<= n 0) '(:json-false) `(t ,limit ,n)))))
    (trx-request-async nil "torrent-set" arguments)))

(defun trx-torrent-honors-speed-limits-p ()
  "Return non-nil if torrent honors session speed limits, otherwise nil."
  (eq t (cdr (assq 'honorsSessionLimits (elt trx-torrent-vector 0)))))

(defun trx-refresh-session-cache ()
  "Update `trx-session-cache' from the Transmission daemon."
  (trx-request-async
   (lambda (response) (setq trx-session-cache response))
   "session-get"
   '(:fields ["speed-limit-up" "speed-limit-down"
              "speed-limit-up-enabled" "speed-limit-down-enabled"
              "seedRatioLimit" "seedRatioLimited"
              "alt-speed-up" "alt-speed-down"
              "alt-speed-time-day" "alt-speed-time-enabled"
              "alt-speed-time-begin" "alt-speed-time-end"])))

(defun trx-prompt-speed-limit (upload)
  "Make a prompt to set transfer speed limit.
If UPLOAD is non-nil, make a prompt for upload rate, otherwise
for download rate."
  (let-alist (or trx-session-cache
                 (trx-request "session-get"
                              '(:fields ["speed-limit-up" "speed-limit-down"
                                         "speed-limit-up-enabled"
                                         "speed-limit-down-enabled"])))
    (let ((limit (if upload .speed-limit-up .speed-limit-down))
          (enabled (eq t (if upload .speed-limit-up-enabled
                           .speed-limit-down-enabled))))
      (list (read-number (concat "Set global " (if upload "up" "down") "load limit ("
                                 (if enabled (format "%d kB/s" limit) "disabled")
                                 "): "))))))

(defun trx-prompt-ratio-limit ()
  "Make a prompt to set global seed ratio limit."
  (let-alist (or trx-session-cache
                 (trx-request "session-get"
                              '(:fields ["seedRatioLimit" "seedRatioLimited"])))
    (let ((limit .seedRatioLimit)
          (enabled (eq t .seedRatioLimited)))
      (list (read-number (concat "Set global seed ratio limit ("
                                 (if enabled (format "%.1f" limit) "disabled")
                                 "): "))))))

(defun trx-read-strings (prompt &optional collection history filter)
  "Read strings until an input is blank, with optional completion.
PROMPT, COLLECTION, and HISTORY are the same as in `completing-read'.
FILTER is a predicate that prevents adding failing input to HISTORY.
Returns a list of non-blank inputs."
  (let ((history-add-new-input (null history))
        res entry)
    (while (and (setq entry (if (not collection) (read-string prompt nil history)
                              (completing-read prompt collection nil nil nil history)))
                (not (string-empty-p entry))
                (not (string-blank-p entry)))
      (when (and history (or (null filter) (funcall filter entry)))
        (add-to-history history entry))
      (push entry res)
      (when (consp collection)
        (setq collection (delete entry collection))))
    (nreverse res)))

(defun trx-read-time (prompt)
  "Read an expression for time, prompting with string PROMPT.
Uses `diary-entry-time' to parse user input.
Returns minutes from midnight, otherwise nil."
  (let ((hhmm (diary-entry-time (read-string prompt))))
    (when (>= hhmm 0) (+ (% hhmm 100) (* 60 (/ hhmm 100))))))

(defun trx-format-minutes (minutes)
  "Return a formatted string from MINUTES from midnight."
  (format-time-string "%H:%M" (seconds-to-time (* 60 (+ 300 minutes)))))

(defun trx-n->days (n)
  "Return days corresponding to bitfield N.
Days are the keys of `trx-schedules'."
  (cond
   ((let ((cell (rassq n trx-schedules)))
      (when cell (list (car cell)))))
   ((let (res)
      (pcase-dolist (`(,k . ,v) trx-schedules)
        (unless (zerop (logand n v))
          (push k res)
          (cl-decf n v)))
      (nreverse res)))))

(defun trx-levi-civita (a b c)
  "Return Levi-Civita symbol value for three numbers A, B, C."
  (cond
   ((or (< a b c) (< b c a) (< c a b)) 1)
   ((or (< c b a) (< a c b) (< b a c)) -1)
   ((or (= a b) (= b c) (= c a)) 0)))

(defun trx-turtle-when (beg end &optional now)
  "Calculate the time in seconds until the next schedule change.
BEG END are minutes after midnight of schedules start and end.
NOW is a time, defaulting to `current-time'."
  (let* ((time (or now (current-time)))
         (hours (string-to-number (format-time-string "%H" time)))
         (minutes (+ (* 60 hours)
                     (string-to-number (format-time-string "%M" time)))))
    (pcase (trx-levi-civita minutes beg end)
      (1 (* 60 (if (> beg minutes) (- beg minutes) (+ beg minutes))))
      (-1 (* 60 (if (> end minutes) (- end minutes) (+ end minutes))))
      ;; FIXME this should probably just return 0 because of inaccuracy
      (0 (* 60 (or (and (= minutes beg) end) (and (= minutes end) beg)))))))

(defun trx-tracker-url-p (str)
  "Return non-nil if STR is not just a number."
  (let ((match (string-match "[^[:blank:]]" str)))
    (when match (null (<= ?0 (aref str match) ?9)))))

(defun trx-tracker-stats (id)
  "Return the \"trackerStats\" array for torrent id ID."
  (let* ((arguments `(:ids ,id :fields ["trackerStats"]))
         (response (trx-request "torrent-get" arguments)))
    (cdr (assq 'trackerStats (elt (trx-torrents response) 0)))))

(defun trx-unique-announce-urls ()
  "Return a list of unique announce URLs from all current torrents."
  (let ((response (trx-request "torrent-get" '(:fields ["trackers"])))
        torrents trackers res)
    (dotimes (i (length (setq torrents (trx-torrents response))))
      (dotimes (j (length (setq trackers (cdr (assq 'trackers (aref torrents i))))))
        (cl-pushnew (cdr (assq 'announce (aref trackers j))) res :test #'equal)))
    res))

(defun trx-btih-p (string)
  "Return STRING if it is a BitTorrent info hash, otherwise nil."
  (and string (string-match (rx bos (= 40 xdigit) eos) string) string))

(defun trx-directory-name-p (name)
  "Return non-nil if NAME ends with a directory separator character."
  (let ((len (length name))
        (last ?.))
    (if (> len 0) (setq last (aref name (1- len))))
    (or (= last ?/)
        (and (memq system-type '(windows-nt ms-dos))
             (= last ?\\)))))

(defun trx-ffap ()
  "Return a file name, URL, or info hash at point, otherwise nil."
  (or (get-text-property (point) 'shr-url)
      (get-text-property (point) :nt-link)
      (let ((fn (run-hook-with-args-until-success 'file-name-at-point-functions)))
        (unless (trx-directory-name-p fn) fn))
      (url-get-url-at-point)
      (trx-btih-p (thing-at-point 'word))))

(defun trx-ffap-string (string)
  "Apply `trx-ffap' to the beginning of STRING."
  (when string
    (with-temp-buffer
      (insert string)
      (goto-char (point-min))
      (trx-ffap))))

(defun trx-ffap-last-killed ()
  "Apply `trx-ffap' to the most recent `kill-ring' entry."
  (trx-ffap-string (car kill-ring)))

(defun trx-ffap-selection ()
  "Apply `trx-ffap' to the graphical selection."
  (trx-ffap-string (with-no-warnings (x-get-selection))))

(defun trx-files-do (action)
  "Apply ACTION to files in `trx-files-mode' buffers."
  (cl-assert (memq action trx-file-symbols))
  (let ((id trx-torrent-id)
        (prop 'tabulated-list-id)
        indices)
    (setq indices
          (or trx-marked-ids
              (if (null (use-region-p))
                  (list (cdr (assq 'index (get-text-property (point) prop))))
                (trx-refs (trx-text-property-all
                                    (region-beginning) (region-end) prop)
                                   'index))))
    (if (and id indices)
        (let ((arguments (list :ids id action indices)))
          (trx-request-async nil "torrent-set" arguments))
      (user-error "No files selected or at point"))))

(defun trx-files-file-at-point ()
  "Return the absolute path of the torrent file at point, or nil.
If the file named \"foo\" does not exist, try \"foo.part\" before returning."
  (let* ((dir (cdr (assq 'downloadDir (elt trx-torrent-vector 0))))
         (base (or (and dir (cdr (assq 'name (tabulated-list-get-id))))
                   (user-error "No file at point")))
         (filename (and base (expand-file-name base dir))))
    (or (file-exists-p filename)
        (let ((part (concat filename ".part")))
          (and (file-exists-p part) (setq filename part))))
    (if filename (abbreviate-file-name filename)
      (user-error "File does not exist"))))

(defun trx-files-index (torrent)
  "Return an array containing file data from TORRENT."
  (let-alist torrent
    (let* ((n (length .files))
           (res (make-vector n 0)))
      (dotimes (i n)
        (aset res i (append (aref .files i)
                            (list (cons 'wanted (aref .wanted i))
                                  (cons 'priority (aref .priorities i))
                                  (cons 'index i)))))
      res)))

(defun trx-files-prefix (files)
  "Return a directory name that is a prefix of every path in FILES, otherwise nil."
  (when (> (length files) 0)
    (let ((ref (cdr (assq 'name (aref files 0))))
          (start 0)
          end)
      (setq files (substring files 1))
      (while (and (prog1 (string-match "/" ref start)
                    (setq end (match-end 0)))
                  (cl-loop for file across files
                           always (eq t (compare-strings
                                         ref start end (cdr (assq 'name file)) start end))))
        (setq start end))
      (substring ref 0 start))))

(defun trx-geoiplookup (ip)
  "Return country name associated with IP using geoiplookup(1)."
  (let ((program (if (string-match-p ":" ip) "geoiplookup6" "geoiplookup")))
    (when (executable-find program)
      (with-temp-buffer
        (call-process program nil t nil ip)
        (car (last (split-string (buffer-string) ": " t "[ \t\r\n]*")))))))

(defun trx-geoip-retrieve (ip)
  "Retrieve value of IP in `trx-geoip-table'.
If IP is not a key, add it with the value from `trx-geoip-function'.
If `trx-geoip-function' has changed, reset `trx-geoip-table'."
  (let ((fun trx-geoip-function)
        (cache trx-geoip-table))
    (when (functionp fun)
      (if (not trx-geoip-use-cache)
          (funcall fun ip)
        (if (eq fun (get 'trx-geoip-table :fn))
            (or (gethash ip cache)
                (setf (gethash ip cache) (funcall fun ip)))
          (setq cache (make-hash-table :test 'equal))
          (put 'trx-geoip-table :fn fun)
          (setf (gethash ip cache) (funcall fun ip)))))))

(defun trx-time (seconds)
  "Format a time string, given SECONDS from the epoch."
  (if (= 0 seconds) "Never"
    (format-time-string trx-time-format (seconds-to-time seconds)
                        trx-time-zone)))

(defun trx-hamming-weight (byte)
  "Calculate the Hamming weight of BYTE."
  (setq byte (- byte (logand (ash byte -1) #x55555555)))
  (setq byte (+ (logand byte #x33333333) (logand (ash byte -2) #x33333333)))
  (ash (* (logand (+ byte (ash byte -4)) #x0f0f0f0f) #x01010101) -24))

(defun trx-count-bits (bytearray)
  "Calculate sum of Hamming weight of each byte in BYTEARRAY."
  (cl-loop for x across bytearray sum (trx-hamming-weight x)))

(defun trx-byte->string (byte)
  "Format integer BYTE into a string."
  (let* ((calc-number-radix 2)
         (string (math-format-binary byte)))
    (concat (make-string (- 8 (length string)) ?0) string)))

(defun trx-ratio->glyph (ratio)
  "Return a single-char string representing RATIO."
  (cond
   ((= 0 ratio) " ")
   ((< ratio 0.333) "\u2591")
   ((< ratio 0.667) "\u2592")
   ((< ratio 1) "\u2593")
   ((= 1 ratio) "\u2588")))

(defun trx-ratio->256 (ratio)
  "Return a grey font-locked single-space string according to RATIO.
Uses color names for the 256 color palette."
  (let ((n (if (= 1 ratio) 231 (+ 236 (* 19 ratio)))))
    (propertize " " 'font-lock-face `(:background ,(format "color-%d" n)))))

(defun trx-ratio->grey (ratio)
  "Return a grey font-locked single-space string according to RATIO."
  (let ((l (+ 0.2 (* 0.8 ratio))))
    (propertize " " 'font-lock-face `(:background ,(color-rgb-to-hex l l l))
                'help-echo (format "%.2f" ratio))))

(defun trx-group-digits (n)
  "Group digits of positive number N with `trx-digit-delimiter'."
  (if (< n 10000) (number-to-string n)
    (let ((calc-group-char trx-digit-delimiter))
      (math-group-float (number-to-string n)))))

(defvar-local trx--fade-overlays nil
  "Overlays for the truncation fade effect.")

(defun trx--truncate (string max-width &optional face)
  "Truncate STRING to MAX-WIDTH with optional FACE.
Mark the last three characters of truncated text with the
`trx-fade' property for post-rendering color blending.
FACE is applied as both `face' and `font-lock-face'."
  (let* ((truncated (> (string-width string) max-width))
         (result (if truncated
                     (truncate-string-to-width string max-width)
                   string)))
    (when face
      (setq result (propertize result 'face face 'font-lock-face face))
      (when truncated
        (let ((len (length result)))
          (when (>= len 3)
            (dotimes (i 3)
              (put-text-property (+ (- len 3) i) (+ (- len 3) i 1)
                                'trx-fade (1+ i) result))))))
    result))

(defun trx--blend-color (fg bg ratio)
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

(defun trx--resolve-foreground (face-val)
  "Resolve the effective foreground color from FACE-VAL."
  (cond
   ((symbolp face-val)
    (face-foreground face-val nil t))
   ((consp face-val)
    (cl-some (lambda (f)
               (and (facep f) (face-foreground f nil t)))
             face-val))))

(defun trx--apply-fades ()
  "Create fade overlays for characters marked with `trx-fade'."
  (mapc #'delete-overlay trx--fade-overlays)
  (setq trx--fade-overlays nil)
  (let ((bg (face-background 'default nil t)))
    (when bg
      (save-excursion
        (goto-char (point-min))
        (let ((pos (point-min)))
          (while (< pos (point-max))
            (let ((level (get-text-property pos 'trx-fade)))
              (if (not level)
                  (setq pos (or (next-single-property-change
                                 pos 'trx-fade nil (point-max))
                                (point-max)))
                (let* ((face-val (or (get-text-property pos 'font-lock-face)
                                     (get-text-property pos 'face)))
                       (fg (or (trx--resolve-foreground face-val)
                               (face-foreground 'default nil t)))
                       (ratio (* 0.25 level))
                       (blended (when fg
                                  (trx--blend-color fg bg ratio))))
                  (when blended
                    (let ((ov (make-overlay pos (1+ pos))))
                      (overlay-put ov 'face (list :foreground blended))
                      (overlay-put ov 'trx-fade t)
                      (push ov trx--fade-overlays))))
                (setq pos (1+ pos))))))))))

(defun trx-plural (n s)
  "Return a pluralized string expressing quantity N of thing S.
Done in the spirit of `dired-plural-s'."
  (let ((m (if (= -1 n) 0 n)))
    (concat (trx-group-digits m) " " s (when (/= m 1) "s"))))

(defun trx-format-size (bytes)
  "Format size BYTES into a more readable string."
  (format "%s (%s bytes)" (trx-size bytes)
          (trx-group-digits bytes)))

(defun trx-toggle-mark-at-point ()
  "Toggle mark of item at point.
Registers the change in `trx-marked-ids'."
  (let* ((eid (tabulated-list-get-id))
         (id (cdr (or (assq 'hashString eid) (assq 'index eid)))))
    (if (member id trx-marked-ids)
        (progn
          (setq trx-marked-ids (delete id trx-marked-ids))
          (tabulated-list-put-tag " "))
      (push id trx-marked-ids)
      (tabulated-list-put-tag ">"))
    (set-buffer-modified-p nil)))

(defun trx-move-to-file-name ()
  "Move to the beginning of the filename on the current line."
  (let* ((eol (line-end-position))
         (change (next-single-property-change (point) 'trx-name nil eol)))
    (when (and change (< change eol))
      (goto-char change))))

(defun trx-file-name-matcher (limit)
  (let ((beg (next-single-property-change (point) 'trx-name nil limit)))
    (when (and beg (< beg limit))
      (goto-char beg)
      (let ((end (next-single-property-change (point) 'trx-name nil limit)))
        (when (and end (<= end limit))
          (set-match-data (list beg end))
          (goto-char end))))))

(defmacro trx-interactive (&rest spec)
  "Specify interactive use of a function.
The symbol `ids' is bound to a list of torrent IDs marked, at
point or in region, otherwise a `user-error' is signalled."
  (declare (debug t))
  (let ((region (make-symbol "region"))
        (marked (make-symbol "marked"))
        (torrent (make-symbol "torrent")))
    `(interactive
      (let ((,torrent trx-torrent-id) ,marked ,region ids)
        (setq ids (or (and ,torrent (list ,torrent))
                      (setq ,marked trx-marked-ids)))
        (when (null ids)
          (if (setq ,region (use-region-p))
              (setq ids
                    (cl-loop for x in
                     (trx-text-property-all
                      (region-beginning) (region-end) 'tabulated-list-id)
                     collect (cdr (assq 'hashString x))))
            (let ((value (tabulated-list-get-id (point))))
              (when value (setq ids (list (cdr (assq 'hashString value))))))))
        (if (null ids) (user-error "No torrent selected")
          ,@(cl-labels
                ((expand (form x)
                   (cond
                    ((atom form) form)
                    ((and (listp form)
                          (memq (car form)
                                '(read-number y-or-n-p yes-or-no-p
                                  completing-read trx-read-strings)))
                     (pcase form
                       (`(read-number ,prompt . ,rest)
                        `(read-number (concat ,prompt ,x) ,@rest))
                       (`(y-or-n-p ,prompt)
                        `(y-or-n-p (concat ,prompt ,x)))
                       (`(yes-or-no-p ,prompt)
                        `(yes-or-no-p (concat ,prompt ,x)))
                       (`(completing-read ,prompt . ,rest)
                        `(completing-read (concat ,prompt ,x) ,@rest))
                       (`(trx-read-strings ,prompt . ,rest)
                        `(trx-read-strings (concat ,prompt ,x) ,@rest))))
                    ((or (listp form) (null form))
                     (mapcar (lambda (subexp) (expand subexp x)) form))
                    (t (error "Bad syntax: %S" form)))))
              (expand spec
                      `(cond
                        (,marked (format "[%d marked] " (length ,marked)))
                        (,region (format "[%d in region] " (length ids)))))))))))

(defun trx-collect-hook (hook &rest args)
  "Run HOOK with ARGS and return a list of non-nil results from its elements."
  (let (res)
    (cl-flet
        ((collect (fun &rest args)
           (let ((val (apply fun args)))
             (when val (cl-pushnew val res :test #'equal))
             nil)))
      (apply #'run-hook-wrapped hook #'collect args)
      (nreverse res))))

(defmacro trx-with-window-maybe (window &rest body)
  "If WINDOW is non-nil, execute BODY with WINDOW current.
Otherwise, just execute BODY."
  (declare (indent 1) (debug t))
  `(if (null ,window) (progn ,@body)
     (with-selected-window ,window
       ,@body)))

(defun trx-window->state (window)
  "Return a list containing some state of WINDOW.
A simplification of `window-state-get', the list associates
WINDOW with `window-start' and the line/column coordinates of `point'."
  (trx-with-window-maybe window
    (save-restriction
      (widen)
      (list window (window-start) (line-number-at-pos) (current-column)))))

(defun trx-restore-state (state)
  "Set `window-start' and `window-point' according to STATE."
  (pcase-let ((`(,window ,start ,line ,column) state))
    (trx-with-window-maybe window
      (goto-char (point-min))
      (forward-line (1- line))
      (move-to-column column)
      (setf (window-start) start))))

(defmacro trx-with-saved-state (&rest body)
  "Execute BODY, restoring window position, point, and mark."
  (declare (indent 0) (debug t))
  (let ((old-states (make-symbol "old-states"))
        (old-mark (make-symbol "old-mark"))
        (old-mark-active (make-symbol "old-mark-active")))
    `(let* ((,old-states (or (mapcar #'trx-window->state
                                     (get-buffer-window-list nil nil t))
                             (list (trx-window->state nil))))
            (,old-mark (if (not (region-active-p)) (mark)
                         (let ((beg (region-beginning)))
                           (if (= (window-point) beg) (region-end) beg))))
            (,old-mark-active mark-active))
       ,@body
       (mapc #'trx-restore-state ,old-states)
       (and ,old-mark (set-mark ,old-mark))
       (unless ,old-mark-active (deactivate-mark)))))


;; Interactive

;;;###autoload
(defun trx-add (torrent &optional directory)
  "Add TORRENT by filename, URL, magnet link, or info hash.
When called with a prefix, prompt for DIRECTORY."
  (interactive
   (let* ((f (trx-collect-hook 'trx-torrent-functions))
          (def (mapcar #'file-relative-name f))
          (prompt (concat "Add torrent" (if def (format " [%s]" (car def))) ": "))
          (history-add-new-input nil)
          (file-name-history (symbol-value trx-add-history-variable))
          (input (if (trx--uri-like-p (car def))
                     (read-string prompt (car def)
                                  trx-add-history-variable)
                   (read-file-name prompt nil def))))
     (add-to-history trx-add-history-variable input)
     (list input
           (if current-prefix-arg
               (read-directory-name "Target directory: ")))))
  (trx-request-async
   (lambda (response)
     (let-alist response
       (or (and .torrent-added.name
                (message "Added %s" .torrent-added.name))
           (and .torrent-duplicate.name
                (message "Already added %s" .torrent-duplicate.name)))))
   "torrent-add"
   (append (if (and (file-readable-p torrent) (not (file-directory-p torrent)))
               `(:metainfo ,(with-temp-buffer
                              (insert-file-contents-literally torrent)
                              (base64-encode-string (buffer-string) t)))
             (setq torrent (string-trim torrent))
             `(:filename ,(if (trx-btih-p torrent)
                              (concat "magnet:?xt=urn:btih:" torrent)
                            torrent)))
           (when directory (list :download-dir (expand-file-name directory))))))

(defun trx--uri-like-p (string)
  "Return non-nil if STRING looks like a URI or magnet link."
  (and (stringp string)
       (string-match-p "\\`\\(?:magnet:\\|https?://\\|udp://\\)" string)))

(defun trx-free (directory)
  "Show in the echo area how much free space is in DIRECTORY."
  (interactive (list (read-directory-name "Directory: " nil nil t)))
  (trx-request-async
   (lambda (response)
     (let-alist response
       (message "%s free in %s" (trx-format-size .size-bytes)
                (abbreviate-file-name .path))))
   "free-space" (list :path (expand-file-name directory))))

(defun trx-stats ()
  "Message some information about the session."
  (interactive)
  (trx-request-async
   (lambda (response)
     (let-alist response
       (message (concat "%d kB/s down, %d kB/s up; %d/%d torrents active; "
                        "%s received, %s sent; uptime %s")
                (trx-rate .downloadSpeed)
                (trx-rate .uploadSpeed)
                .activeTorrentCount .torrentCount
                (trx-size .current-stats.downloadedBytes)
                (trx-size .current-stats.uploadedBytes)
                (trx-eta .current-stats.secondsActive nil))))
   "session-stats"))

(defun trx-move (ids location)
  "Move torrent at point, marked, or in region to a new LOCATION."
  (trx-interactive
   (let* ((dir (read-directory-name "New directory: "))
          (prompt (format "Move torrent%s to %s? " (if (cdr ids) "s" "") dir)))
     (if (y-or-n-p prompt) (list ids dir) '(nil nil))))
  (when ids
    (let ((arguments (list :ids ids :move t :location (expand-file-name location))))
      (trx-request-async nil "torrent-set-location" arguments))))

(defun trx-reannounce (ids)
  "Reannounce torrent at point, marked, or in region."
  (trx-interactive (list ids))
  (when ids
    (trx-request-async nil "torrent-reannounce" (list :ids ids))))

(defun trx-remove (ids &optional unlink)
  "Prompt to remove torrent at point or torrents marked or in region.
When called with a prefix UNLINK, also unlink torrent data on disk."
  (trx-interactive
   (if (yes-or-no-p (concat "Remove " (and current-prefix-arg "and unlink ")
                            "torrent" (and (cdr ids) "s") "? "))
       (progn (setq deactivate-mark t trx-marked-ids nil)
              (list ids current-prefix-arg))
     '(nil nil)))
  (when ids
    (let ((arguments `(:ids ,ids :delete-local-data ,(and unlink t))))
      (trx-request-async nil "torrent-remove" arguments))))

(defun trx-delete (ids)
  "Prompt to delete (unlink) torrent at point or torrents marked or in region."
  (trx-interactive
   (list
    (and (yes-or-no-p (concat "Delete torrent" (and (cdr ids) "s") "? "))
         (setq trx-marked-ids nil deactivate-mark t)
         ids)))
  (when ids
    (trx-request-async nil "torrent-remove" `(:ids ,ids :delete-local-data t))))

(defun trx-set-bandwidth-priority (ids priority)
  "Set bandwidth priority of torrent(s) at point, in region, or marked."
  (trx-interactive
   (let* ((prompt "Set bandwidth priority: ")
          (priority (completing-read prompt trx-priority-alist nil t))
          (number (cdr (assoc-string priority trx-priority-alist))))
     (list (when number ids) number)))
  (when ids
    (let ((arguments `(:ids ,ids :bandwidthPriority ,priority)))
      (trx-request-async nil "torrent-set" arguments))))

(defun trx-set-download (limit)
  "Set global download speed LIMIT in kB/s."
  (interactive (trx-prompt-speed-limit nil))
  (let ((arguments (if (<= limit 0) '(:speed-limit-down-enabled :json-false)
                     `(:speed-limit-down-enabled t :speed-limit-down ,limit))))
    (trx-request-async nil "session-set" arguments)))

(defun trx-set-upload (limit)
  "Set global upload speed LIMIT in kB/s."
  (interactive (trx-prompt-speed-limit t))
  (let ((arguments (if (<= limit 0) '(:speed-limit-up-enabled :json-false)
                     `(:speed-limit-up-enabled t :speed-limit-up ,limit))))
    (trx-request-async nil "session-set" arguments)))

(defun trx-set-ratio (limit)
  "Set global seed ratio LIMIT."
  (interactive (trx-prompt-ratio-limit))
  (let ((arguments (if (< limit 0) '(:seedRatioLimited :json-false)
                     `(:seedRatioLimited t :seedRatioLimit ,limit))))
    (trx-request-async nil "session-set" arguments)))

(defun trx-set-torrent-download (ids)
  "Set download limit of selected torrent(s) in kB/s."
  (trx-interactive (list ids))
  (if (cdr ids)
      (let ((prompt "Set torrents' download limit: "))
        (trx-throttle-torrent ids :downloadLimit (read-number prompt)))
    (trx-request-async
     (lambda (response)
       (let-alist (elt (trx-torrents response) 0)
         (let* ((s (if (eq t .downloadLimited) (format "%d kB/s" .downloadLimit) "disabled"))
                (prompt (concat "Set torrent's download limit (" s "): ")))
           (trx-throttle-torrent ids :downloadLimit (read-number prompt)))))
     "torrent-get" `(:ids ,ids :fields ["downloadLimit" "downloadLimited"]))))

(defun trx-set-torrent-upload (ids)
  "Set upload limit of selected torrent(s) in kB/s."
  (trx-interactive (list ids))
  (if (cdr ids)
      (let ((prompt "Set torrents' upload limit: "))
        (trx-throttle-torrent ids :uploadLimit (read-number prompt)))
    (trx-request-async
     (lambda (response)
       (let-alist (elt (trx-torrents response) 0)
         (let* ((s (if (eq t .uploadLimited) (format "%d kB/s" .uploadLimit) "disabled"))
                (prompt (concat "Set torrent's upload limit (" s "): ")))
           (trx-throttle-torrent ids :uploadLimit (read-number prompt)))))
     "torrent-get" `(:ids ,ids :fields ["uploadLimit" "uploadLimited"]))))

(defun trx-set-torrent-ratio (ids mode limit)
  "Set seed ratio limit of selected torrent(s)."
  (trx-interactive
   (let* ((prompt (concat "Set torrent" (if (cdr ids) "s'" "'s") " ratio mode: "))
          (mode (completing-read prompt trx-mode-alist nil t))
          (n (cdr (assoc-string mode trx-mode-alist))))
     (list ids n (when (= n 1) (read-number "Set torrent ratio limit: ")))))
  (when ids
    (let ((arguments `(:ids ,ids :seedRatioMode ,mode
                       ,@(when limit `(:seedRatioLimit ,limit)))))
      (trx-request-async nil "torrent-set" arguments))))

(defun trx-toggle-limits (ids)
  "Toggle whether selected torrent(s) honor session speed limits."
  (trx-interactive (list ids))
  (when ids
    (trx-request-async
     (lambda (response)
       (let* ((torrents (trx-torrents response))
              (honor (pcase (cdr (assq 'honorsSessionLimits (elt torrents 0)))
                       (:json-false t) (_ :json-false))))
         (trx-request-async nil "torrent-set"
                                     `(:ids ,ids :honorsSessionLimits ,honor))))
     "torrent-get" `(:ids ,ids :fields ["honorsSessionLimits"]))))

(defun trx-toggle (ids)
  "Toggle selected torrent(s) between started and stopped."
  (trx-interactive (list ids))
  (when ids
    (trx-request-async
     (lambda (response)
       (let* ((torrents (trx-torrents response))
              (status (and torrents (cdr (assq 'status (elt torrents 0)))))
              (method (and status
                           (if (zerop status) "torrent-start" "torrent-stop"))))
         (when method (trx-request-async nil method (list :ids ids)))))
     "torrent-get" (list :ids ids :fields ["status"]))))

(defun trx-label (ids labels)
  "Set labels for selected torrent(s)."
  (trx-interactive
   (let* ((response (trx-request "torrent-get" '(:fields ["labels"])))
          (torrents (trx-torrents response)))
     (list ids (trx-read-strings "Labels: " (trx-unique-labels torrents)))))
  (trx-request-async
   nil "torrent-set" (list :ids ids :labels (vconcat labels))))

(defun trx-trackers-add (ids urls)
  "Add announce URLs to selected torrent or torrents."
  (trx-interactive
   (let* ((trackers (trx-refs (trx-tracker-stats ids) 'announce))
          (urls (or (trx-read-strings
                     "Add announce URLs: "
                     (cl-loop for url in
                              (append trx-trackers
                                      (trx-unique-announce-urls))
                              unless (member url trackers) collect url)
                     trx-tracker-history-variable
                     #'trx-tracker-url-p)
                    (user-error "No trackers to add"))))
     (list ids
           ;; Don't add trackers that are already there
           (cl-loop for url in urls
                    unless (member url trackers) collect url))))
  (trx-request-async
   (lambda (_) (message "Added %s" (mapconcat #'identity urls ", ")))
   "torrent-set" (list :ids ids :trackerAdd urls)))

(defun trx-trackers-remove ()
  "Remove trackers from torrent at point by ID or announce URL."
  (interactive)
  (let* ((id (or trx-torrent-id (user-error "No torrent selected")))
         (array (or (trx-tracker-stats id)
                    (user-error "No trackers to remove")))
         (prompt (format "Remove tracker (%d trackers): " (length array)))
         (trackers (cl-loop for x across array
                            collect (cons (cdr (assq 'announce x))
                                          (cdr (assq 'id x)))))
         (completion-extra-properties
          `(:annotation-function
            (lambda (x) (format " ID# %d" (cdr (assoc x ',trackers))))))
         (urls (or (trx-read-strings
                    prompt trackers trx-tracker-history-variable
                    #'trx-tracker-url-p)
                   (user-error "No trackers selected for removal")))
         (tids (cl-loop for alist across array
                        if (or (member (cdr (assq 'announce alist)) urls)
                               (member (number-to-string (cdr (assq 'id alist))) urls))
                        collect (cdr (assq 'id alist)))))
    (trx-request-async
     (lambda (_) (message "Removed %s" (mapconcat #'identity urls ", ")))
     "torrent-set" (list :ids id :trackerRemove tids))))

(defun trx-trackers-replace ()
  "Replace tracker by ID or announce URL."
  (interactive)
  (let* ((id (or trx-torrent-id (user-error "No torrent selected")))
         (trackers (or (cl-loop for x across (trx-tracker-stats id)
                                collect (cons (cdr (assq 'announce x))
                                              (cdr (assq 'id x))))
                       (user-error "No trackers to replace")))
         (prompt (format "Replace tracker (%d trackers): " (length trackers)))
         (tid (or (let* ((completion-extra-properties
                          `(:annotation-function
                            (lambda (x)
                              (format " ID# %d" (cdr (assoc x ',trackers))))))
                         (tracker (completing-read prompt trackers)))
                    (cl-loop for cell in trackers
                             if (member tracker (list (car cell)
                                                      (number-to-string (cdr cell))))
                             return (cdr cell)))
                  (user-error "No tracker selected for substitution")))
         (replacement
          (completing-read "Replacement tracker? "
                           (append trx-trackers
                                   (trx-unique-announce-urls))
                           nil nil nil
                           trx-tracker-history-variable)))
    (trx-request-async
     (lambda (_) (message "Replaced #%d with %s" tid replacement))
     "torrent-set" (list :ids id :trackerReplace (vector tid replacement)))))

(defun trx-turtle-set-days (days &optional disable)
  "Set DAYS on which turtle mode will be active.
DAYS is a bitfield, the associations of which are in `trx-schedules'.
Empty input or non-positive DAYS makes no change to the schedule.
With a prefix argument, disable turtle mode schedule."
  (interactive
   (let ((arguments '(:fields ["alt-speed-time-day" "alt-speed-time-enabled"])))
     (let-alist (trx-request "session-get" arguments)
       (let* ((alist trx-schedules)
              (prompt
               (format "Days %s%s: "
                       (or (trx-n->days .alt-speed-time-day) "(none)")
                       (if (eq t .alt-speed-time-enabled) "" " [disabled]")))
              (names (trx-read-strings prompt alist))
              (bits 0))
         (dolist (name names)
           (cl-callf logior bits (cdr (assq (intern name) alist))))
         (list bits current-prefix-arg)))))
  (let ((arguments
         (append `(:alt-speed-time-enabled ,(if disable json-false t))
                 (when (> days 0) `(:alt-speed-time-day ,days)))))
    (trx-request-async #'trx-turtle-poll "session-set" arguments)))

(defun trx-turtle-set-times (begin end)
  "Set BEGIN and END times for turtle mode.
See `trx-read-time' for details on time input."
  (interactive
   (let ((arguments '(:fields ["alt-speed-time-begin" "alt-speed-time-end"])))
     (let-alist (trx-request "session-get" arguments)
       (let* ((begs (trx-format-minutes .alt-speed-time-begin))
              (ends (trx-format-minutes .alt-speed-time-end))
              (start (or (trx-read-time (format "Begin (%s): " begs))
                         .alt-speed-time-begin))
              (stop (or (trx-read-time (format "End (%s): " ends))
                        .alt-speed-time-end)))
         (when (and (= start .alt-speed-time-begin) (= stop .alt-speed-time-end))
           (user-error "No change in schedule"))
         (if (y-or-n-p (format "Set active time from %s to %s? "
                               (trx-format-minutes start)
                               (trx-format-minutes stop)))
             (list start stop) '(nil nil))))))
  (when (or begin end)
    (let ((arguments
           (append (when begin (list :alt-speed-time-begin begin))
                   (when end (list :alt-speed-time-end end)))))
      (trx-request-async #'trx-turtle-poll "session-set" arguments))))

(defun trx-turtle-set-speeds (up down)
  "Set UP and DOWN speed limits (kB/s) for turtle mode."
  (interactive
   (let-alist (or trx-session-cache
                  (trx-request "session-get"
                               '(:fields ["alt-speed-up" "alt-speed-down"])))
     (let ((p1 (format "Set turtle upload limit (%d kB/s): " .alt-speed-up))
           (p2 (format "Set turtle download limit (%d kB/s): " .alt-speed-down)))
       (list (read-number p1) (read-number p2)))))
  (let ((arguments
         (append (when down (list :alt-speed-down down))
                 (when up (list :alt-speed-up up)))))
    (trx-request-async #'trx-turtle-poll "session-set" arguments)))

(defun trx-turtle-status ()
  "Message details about turtle mode configuration."
  (interactive)
  (trx-request-async
   (lambda (response)
     (let-alist response
       (message
        "%sabled; %d kB/s down, %d kB/s up; schedule %sabled, %s-%s, %s"
        (if (eq .alt-speed-enabled t) "En" "Dis") .alt-speed-down .alt-speed-up
        (if (eq .alt-speed-time-enabled t) "en" "dis")
        (trx-format-minutes .alt-speed-time-begin)
        (trx-format-minutes .alt-speed-time-end)
        (let ((bits (trx-n->days .alt-speed-time-day)))
          (if (null bits) "never" (mapconcat #'symbol-name bits " "))))))
   "session-get"
   '(:fields ["alt-speed-enabled" "alt-speed-down" "alt-speed-up" "alt-speed-time-day"
              "alt-speed-time-enabled" "alt-speed-time-begin" "alt-speed-time-end"])))

(defun trx-verify (ids)
  "Verify torrent at point, in region, or marked."
  (trx-interactive
   (if (y-or-n-p (concat "Verify torrent" (when (cdr ids) "s") "? "))
       (list ids) '(nil)))
  (when ids (trx-request-async nil "torrent-verify" (list :ids ids))))

(defun trx-queue-move-top (ids)
  "Move torrent(s)--at point, in region, or marked--to the top of the queue."
  (trx-interactive
   (if (y-or-n-p (concat "Queue torrent" (when (cdr ids) "s") " first? "))
       (list ids) '(nil)))
  (when ids
    (trx-request-async nil "queue-move-top" (list :ids ids))))

(defun trx-queue-move-bottom (ids)
  "Move torrent(s)--at point, in region, or marked--to the bottom of the queue."
  (trx-interactive
   (if (y-or-n-p (concat "Queue torrent" (when (cdr ids) "s") " last? "))
       (list ids) '(nil)))
  (when ids
    (trx-request-async nil "queue-move-bottom" (list :ids ids))))

(defun trx-queue-move-up (ids)
  "Move torrent(s)--at point, in region, or marked--up in the queue."
  (trx-interactive
   (if (y-or-n-p (concat "Raise torrent" (when (cdr ids) "s") " in the queue? "))
       (list ids) '(nil)))
  (when ids
    (trx-request-async nil "queue-move-up" (list :ids ids))))

(defun trx-queue-move-down (ids)
  "Move torrent(s)--at point, in region, or marked--down in the queue."
  (trx-interactive
   (if (y-or-n-p (concat "Lower torrent" (when (cdr ids) "s") " in the queue? "))
       (list ids) '(nil)))
  (when ids
    (trx-request-async nil "queue-move-down" (list :ids ids))))

(defun trx-quit ()
  "Quit and bury the buffer."
  (interactive)
  (if (let ((cur (current-buffer)))
        (cl-loop for list in (window-prev-buffers) never (eq cur (car list))))
      (quit-window)
    (if (one-window-p)
        (bury-buffer)
      (delete-window))))

(defun trx-daemon-start ()
  "Start the Transmission daemon.
Uses `trx-daemon-program' to locate the executable."
  (interactive)
  (if (not (executable-find trx-daemon-program))
      (user-error "Cannot find %s" trx-daemon-program)
    (start-process "trx-daemon" nil trx-daemon-program)
    (message "Started %s" trx-daemon-program)))

(defun trx-daemon-stop ()
  "Stop the Transmission daemon via the RPC interface."
  (interactive)
  (trx-request-async
   (lambda (_) (message "Transmission daemon stopped"))
   "session-close"))

(defun trx-rename-path (path new-name)
  "Rename the torrent file at point.
PATH is the current file path, NEW-NAME is the desired new name."
  (interactive
   (let* ((entry (tabulated-list-get-id))
          (file-name (or (cdr (assq 'name entry))
                         (user-error "No file at point")))
          (new (read-string (format "Rename '%s' to: "
                                    (file-name-nondirectory file-name))
                            (file-name-nondirectory file-name))))
     (list file-name new)))
  (let ((id trx-torrent-id))
    (trx-request-async
     (lambda (_response)
       (message "Renamed '%s' to '%s'" (file-name-nondirectory path) new-name)
       (dolist (buf (buffer-list))
         (when (and (buffer-live-p buf)
                    (string-match-p "\\`\\*trx-files:" (buffer-name buf)))
           (with-current-buffer buf
             (when (equal trx-torrent-id id)
               (revert-buffer))))))
     "torrent-rename-path"
     (list :ids (vector id) :path path :name new-name))))

(defun trx-files-unwant ()
  "Mark file(s)--at point, in region, or marked--as unwanted."
  (interactive)
  (trx-files-do :files-unwanted))

(defun trx-files-want ()
  "Mark file(s)--at point, in region, or marked--as wanted."
  (interactive)
  (trx-files-do :files-wanted))

(defun trx-files-priority (priority)
  "Set bandwidth PRIORITY on file(s) at point, in region, or marked."
  (interactive
   (list (completing-read "Set priority: " trx-priority-alist nil t)))
  (trx-files-do (intern (concat ":priority-" priority))))

(defun trx-files-command (command file)
  "Run a command COMMAND on the FILE at point."
  (interactive
   (let* ((fap (run-hook-with-args-until-success 'file-name-at-point-functions))
          (fn (replace-regexp-in-string "\\.part\\'" "" fap))
          (def (let ((lists (trx-collect-hook
                             'trx-files-command-functions (list fn))))
                 (delete-dups (apply #'append lists))))
          (prompt (and fap (concat "! on " (file-name-nondirectory fap)
                                   (when def (format " (default %s)" (car def)))
                                   ": ")))
          (input (read-shell-command prompt nil nil def t)))
     (if fap (list (if (string-empty-p input) (or (car def) "") input) fap)
       (user-error "File does not exist"))))
  (let ((args (nconc (split-string-and-unquote command)
                     (list (expand-file-name file)))))
    (apply #'start-process (car args) nil args)))

(defun trx-copy-file (file newname &optional ok-if-already-exists)
  "Copy the file at point to another location.
FILE, NEWNAME, and OK-IF-ALREADY-EXISTS are the same as in `copy-file'."
  (interactive
   (let* ((f (trx-files-file-at-point))
          (prompt (format "Copy %s to: " (file-name-nondirectory f)))
          (def (when (bound-and-true-p dired-dwim-target)
                 (buffer-local-value 'default-directory
                                     (window-buffer (next-window)))))
          (new (read-file-name prompt nil def)))
     (list f new 0)))
  (copy-file file newname ok-if-already-exists t t t)
  (message "Copied %s" (file-name-nondirectory file)))

(defun trx-find-file ()
  "Visit the file at point with `find-file-read-only'."
  (interactive)
  (find-file-read-only (trx-files-file-at-point)))

(defun trx-find-file-other-window ()
  "Visit the file at point in another window."
  (interactive)
  (find-file-read-only-other-window (trx-files-file-at-point)))

(defun trx-display-file ()
  "Display the file at point in another window."
  (interactive)
  (let ((buf (find-file-noselect (trx-files-file-at-point))))
    (with-current-buffer buf
      (read-only-mode 1))
    (display-buffer buf t)))

(defun trx-view-file ()
  "Examine the file at point in view mode."
  (interactive)
  (view-file (trx-files-file-at-point)))

(defun trx-browse-url-of-file ()
  "Browse file at point in a WWW browser."
  (interactive)
  (browse-url-of-file (expand-file-name (trx-files-file-at-point))))

(defun trx-dired-file ()
  "Show file at point in DirEd."
  (interactive)
  (let* ((f (trx-files-file-at-point))
         (dir (file-name-directory f)))
    (if (file-directory-p dir)
        (with-current-buffer (dired dir)
          (dired-goto-file (expand-file-name f)))
      (message "Directory '%s' does not exist."
               (file-name-base (substring dir 0 -1))))))

(defun trx-copy-filename-as-kill (&optional arg)
  "Copy name of file at point into the kill ring.
With a prefix argument, use the absolute file name."
  (interactive "P")
  (let* ((fn (trx-files-file-at-point))
         (str (if arg fn (file-name-nondirectory fn))))
    (if (eq last-command 'kill-region)
        (kill-append str nil)
      (kill-new str))
    (message "%S" str)))

(defun trx-copy-magnet ()
  "Copy magnet link of current torrent."
  (interactive)
  (let ((magnet (cdr (assq 'magnetLink (elt trx-torrent-vector 0)))))
    (when magnet
      (kill-new magnet)
      (message "Copied %s" magnet))))

(defun trx-toggle-mark (arg)
  "Toggle mark of item(s) at point.
If the region is active, toggle the mark on all items in the region.
Otherwise, with a prefix arg, mark files on the next ARG lines."
  (interactive "p")
  (if (use-region-p)
      (save-excursion
        (save-restriction
          (narrow-to-region (region-beginning) (region-end))
          (goto-char (point-min))
          (while (not (eobp))
            (trx-toggle-mark-at-point)
            (forward-line))))
    (while (and (> arg 0) (not (eobp)))
      (cl-decf arg)
      (trx-toggle-mark-at-point)
      (forward-line 1))
    (while (and (< arg 0) (not (bobp)))
      (cl-incf arg)
      (forward-line -1)
      (trx-toggle-mark-at-point))))

(defun trx-unmark-all ()
  "Remove mark from all items."
  (interactive)
  (let ((inhibit-read-only t) (n 0))
    (when trx-marked-ids
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (not (eobp))
            (when (= (following-char) ?>)
              (save-excursion
                (forward-char)
                (insert-and-inherit ?\s))
              (delete-region (point) (1+ (point)))
              (cl-incf n))
            (forward-line))))
      (setq trx-marked-ids nil)
      (set-buffer-modified-p nil)
      (message "%s removed" (trx-plural n "mark")))))

(defun trx-invert-marks ()
  "Toggle mark on all items."
  (interactive)
  (let ((inhibit-read-only t) ids tag key)
    (when (setq key (cl-ecase major-mode
                      (trx-mode 'hashString)
                      (trx-files-mode 'index)))
      (save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (while (not (eobp))
            (when (setq tag (car (memq (following-char) '(?> ?\s))))
              (save-excursion
                (forward-char)
                (insert-and-inherit (if (= tag ?>) ?\s ?>)))
              (delete-region (point) (1+ (point)))
              (when (= tag ?\s)
                (push (cdr (assq key (tabulated-list-get-id))) ids)))
            (forward-line))))
      (setq trx-marked-ids ids)
      (set-buffer-modified-p nil))))


;; Turtle mode

(defvar trx-turtle-poll-callback
  (let (timer enabled next lighter)
    (lambda (response)
      (let-alist response
        (setq enabled (eq t .alt-speed-enabled))
        (setq next (trx-turtle-when .alt-speed-time-begin
                                             .alt-speed-time-end))
        (set-default 'trx-turtle-mode enabled)
        (setq lighter
              (if enabled
                  (concat trx-turtle-lighter
                          (format ":%d/%d" .alt-speed-down .alt-speed-up))
                nil))
        (trx-register-turtle-mode lighter)
        (when timer (cancel-timer timer))
        (setq timer (run-at-time next nil #'trx-turtle-poll)))))
  "Closure checking turtle mode status and marshaling a timer.")

(defun trx-turtle-poll (&rest _args)
  "Initiate `trx-turtle-poll-callback' timer function."
  (trx-request-async
   trx-turtle-poll-callback "session-get"
   '(:fields ["alt-speed-enabled" "alt-speed-down" "alt-speed-up"
              "alt-speed-time-begin" "alt-speed-time-end"])))

(defvar trx-turtle-mode-lighter nil
  "Lighter for `trx-turtle-mode'.")

(define-minor-mode trx-turtle-mode
  "Toggle alternative speed limits (turtle mode).
Indicates on the mode-line the down/up speed limits in kB/s."
  :group 'trx
  :global t
  :lighter trx-turtle-mode-lighter
  (trx-request-async
   #'trx-turtle-poll
   "session-set" `(:alt-speed-enabled ,(or trx-turtle-mode json-false))))

(defun trx-register-turtle-mode (lighter)
  "Add LIGHTER to buffers with a trx-* major mode."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (string-prefix-p "trx" (symbol-name major-mode))
        (setq-local trx-turtle-mode-lighter lighter)))))


;; Formatting

(defun trx-format-status (status up down)
  "Return a propertized string describing torrent status.
STATUS is the index of `trx-status-names'.  UP and DOWN are
trx rates."
  (let ((state (aref trx-status-names status))
        (idle (propertize "idle" 'font-lock-face 'shadow))
        (uploading
         (propertize "uploading" 'font-lock-face 'font-lock-constant-face)))
    (pcase status
      (0 (propertize state 'font-lock-face 'warning))
      ((or 1 3 5) (propertize state 'font-lock-face '(bold shadow)))
      (2 (propertize state 'font-lock-face 'font-lock-function-name-face))
      (4 (if (> down 0) (propertize state 'font-lock-face 'highlight)
           (if (> up 0) uploading idle)))
      (6 (if (> up 0) (propertize state 'font-lock-face 'success) idle))
      (_ state))))

(defun trx-format-pieces (pieces count)
  "Format into a string the bitfield PIECES holding COUNT boolean flags."
  (let* ((bytes (base64-decode-string pieces))
         (bits (mapconcat #'trx-byte->string bytes "")))
    (cl-flet ((string-partition (s n)
                (let (res middle last)
                  (while (not (zerop (setq last (length s))))
                    (setq middle (min n last))
                    (push (substring s 0 middle) res)
                    (setq s (substring s middle last)))
                  (nreverse res))))
      (string-join (string-partition (substring bits 0 count) 72) "\n"))))

(defun trx-format-pieces-brief (pieces count)
  "Format pieces into a one-line greyscale representation.
PIECES and COUNT are the same as in `trx-format-pieces'."
  (let* ((bytes (base64-decode-string pieces))
         (slices (trx-slice bytes 72))
         (ratios
          (cl-loop for bv in slices with div = nil
                   do (cl-decf count (setq div (min count (* 8 (length bv)))))
                   collect (/ (trx-count-bits bv) (float div)))))
    (mapconcat (pcase (display-color-cells)
                 ((pred (< 256)) #'trx-ratio->grey)
                 (256 #'trx-ratio->256)
                 (_ #'trx-ratio->glyph))
               ratios "")))

(defun trx-format-pieces-internal (pieces count size)
  "Format piece data into a string.
PIECES and COUNT are the same as in `trx-format-pieces'.
SIZE is the file size in bytes of a single piece."
  (let ((have (cl-loop for b across (base64-decode-string pieces)
                       sum (trx-hamming-weight b))))
    (concat
     "Piece count: " (trx-group-digits have)
     " / " (trx-group-digits count)
     " (" (format "%.1f" (trx-percent have count)) "%) * "
     (trx-format-size size) " each"
     (when (and (functionp trx-pieces-function)
                (/= have 0) (< have count))
       (let ((str (funcall trx-pieces-function pieces count)))
         (concat "\nPieces:\n\n" str))))))

(defun trx-format-ratio (ratio mode limit)
  "String showing a torrent's seed ratio limit.
MODE is which seed ratio to use; LIMIT is the torrent-level limit."
  (concat "Ratio: " (pcase ratio
                      (-2 (if (char-displayable-p #x221e) "\u221e" "Inf"))
                      (-1 "n/a")
                      (_ (format "%.3f" ratio)))
          " / " (pcase mode
                  (0 "session limit")
                  (1 (format "%.2f (torrent-specific limit)" limit))
                  (2 "unlimited"))))

(defun trx-format-peers (peers origins connected sending receiving)
  "Format peer information into a string.
PEERS is an array of peer-specific data.
ORIGINS is an alist giving counts of peers from different swarms.
CONNECTED, SENDING, RECEIVING are numbers."
  (cl-macrolet ((accumulate (array key)
                  `(cl-loop for alist across ,array
                            count (eq t (cdr (assq ,key alist))))))
    (if (zerop connected) "Peers: none connected\n"
      (concat
       (format "Peers: %d connected, uploading to %d, downloading from %d"
               connected sending receiving)
       (format " (%d unchoked, %d interested)\n"
               (- connected (accumulate peers 'clientIsChoked))
               (accumulate peers 'peerIsInterested))
       (format
        "Peer origins: %s\n"
        (string-join
         (cl-loop with x = 0 for cell in origins for src across
                  ["cache" "DHT" "incoming" "LPD" "LTEP" "PEX" "tracker(s)"]
                  if (not (zerop (setq x (cdr cell))))
                  collect (format "%d from %s" x src))
         ", "))))))

(defun trx-format-tracker (tracker)
  "Format alist TRACKER into a string of tracker info."
  (let-alist tracker
    (let* ((label (format "Tracker %d" .id))
           (col (length label))
           (fill (propertize (make-string col ?\s) 'display `(space :align-to ,col)))
           (result (unless (member .lastAnnounceResult '("Success" ""))
                     (concat "\n" fill ": "
                             (propertize .lastAnnounceResult 'font-lock-face 'warning)))))
      (format
       (concat label ": %s (Tier %d)\n"
               fill ": %s %s. Announcing %s\n"
               fill ": %s, %s, %s %s. Scraping %s"
               result)
       .announce .tier
       (trx-plural .lastAnnouncePeerCount "peer")
       (trx-when .lastAnnounceTime) (trx-when .nextAnnounceTime)
       (trx-plural .seederCount "seeder")
       (trx-plural .leecherCount "leecher")
       (trx-plural .downloadCount "download")
       (trx-when .lastScrapeTime) (trx-when .nextScrapeTime)))))

(defun trx-format-trackers (trackers)
  "Format tracker information into a string.
TRACKERS should be the \"trackerStats\" array."
  (if (zerop (length trackers)) "Trackers: none\n"
    (concat (mapconcat #'trx-format-tracker trackers "\n") "\n")))

(defun trx-format-speed-limit (speed limit limited)
  "Format speed limit data into a string.
SPEED and LIMIT are rates in bytes per second.  LIMITED, if t,
indicates that the speed limit is enabled."
  (cond
   ((not (eq limited t)) (format "%d kB/s" (trx-rate speed)))
   (t (format "%d / %d kB/s" (trx-rate speed) limit))))

(defun trx-format-limits (session rx tx rx-lim tx-lim rx-thr tx-thr)
  "Format download and upload rate and limits into a string."
  (concat (trx-format-speed-limit rx rx-lim rx-thr) " down, "
          (trx-format-speed-limit tx tx-lim tx-thr) " up"
          (when (eq session t) ", session limited")))


;; Drawing

(defun trx-tabulated-list-format (&optional _arg _noconfirm)
  "Initialize tabulated-list header or update `tabulated-list-format'."
  (let ((idx (cl-loop for format across tabulated-list-format
                      if (plist-get (nthcdr 3 format) :trx-size)
                      return format))
        (col (if (eq 'iec trx-units) 9 7)))
    (if (= (cadr idx) col)
        (or header-line-format (tabulated-list-init-header))
      (setf (cadr idx) col)
      (tabulated-list-init-header))))

(defmacro trx-do-entries (seq &rest body)
  "Map over SEQ to generate a new value of `tabulated-list-entries'.
Each form in BODY is a column descriptor."
  (declare (indent 1) (debug t))
  (let ((res (make-symbol "res")))
    `(let (,res)
       (mapc (lambda (x) (let-alist x (push (list x (vector ,@body)) ,res)))
             ,seq)
       (setq tabulated-list-entries (nreverse ,res)))))

(defun trx-filter-apply (torrents filter)
  "Return TORRENTS matching FILTER.
FILTER is a string.  Prefix `status:' matches status, `label:'
matches labels, `!' negates.  Plain text matches torrent name
case-insensitively."
  (let* ((negate (string-prefix-p "!" filter))
         (filter (if negate (substring filter 1) filter))
         (predicate
          (cond
           ((string-prefix-p "status:" filter)
            (let ((status (substring filter 7)))
              (lambda (torrent)
                (string-match-p status (aref trx-status-names
                                             (cdr (assq 'status torrent)))))))
           ((string-prefix-p "label:" filter)
            (let ((label (substring filter 6)))
              (lambda (torrent)
                (cl-some (lambda (l) (string-match-p label l))
                         (cdr (assq 'labels torrent))))))
           (t
            (let ((pattern (regexp-quote filter)))
              (lambda (torrent)
                (string-match-p pattern (cdr (assq 'name torrent)))))))))
    (cl-remove-if (if negate predicate (lambda (x) (not (funcall predicate x))))
                  torrents)))

(defun trx-filter (filter)
  "Filter the torrent list by FILTER.
Plain text matches name; `status:X' matches status; `label:X'
matches labels; prefix `!' negates."
  (interactive
   (list (read-string (if trx-filter-active
                          (format "Filter [current: %s]: " trx-filter-active)
                        "Filter: ")
                      nil 'trx-filter-history)))
  (setq trx-filter-active (unless (string-empty-p filter) filter))
  (revert-buffer))

(defun trx-filter-clear ()
  "Clear the active torrent list filter."
  (interactive)
  (setq trx-filter-active nil)
  (revert-buffer))

(defun trx-draw-torrents (_id)
  (let* ((arguments `(:fields ,trx-draw-torrents-keys))
         (response (trx-request "torrent-get" arguments)))
    (setq trx-torrent-vector (trx-torrents response)))
  (when trx-filter-active
    (setq trx-torrent-vector
          (trx-filter-apply trx-torrent-vector trx-filter-active)))
  (trx-do-entries trx-torrent-vector
    (propertize (trx-eta .eta .percentDone) 'font-lock-face 'trx-torrent-size)
    (propertize (trx-size .sizeWhenDone) 'font-lock-face 'trx-torrent-size)
    (format "%d%%" (* 100 (if (= 1 .metadataPercentComplete)
                              .percentDone .metadataPercentComplete)))
    (propertize (format "%d" (trx-rate .rateDownload))
                'font-lock-face 'trx-torrent-download)
    (propertize (format "%d" (trx-rate .rateUpload))
                'font-lock-face 'trx-torrent-upload)
    (propertize (format "%.1f" (if (> .uploadRatio 0) .uploadRatio 0))
                'font-lock-face 'trx-torrent-size)
    (if (not (zerop .error)) (propertize "error" 'font-lock-face 'error)
      (trx-format-status .status .rateUpload .rateDownload))
    (propertize (trx-when .addedDate) 'font-lock-face 'trx-torrent-size)
    (concat
     (propertize .name 'font-lock-face 'trx-torrent-name 'trx-name t)
     (mapconcat (lambda (l)
                  (concat " " (propertize l 'font-lock-face 'trx-torrent-label)))
                .labels "")))
  (tabulated-list-print)
  (trx--apply-fades)
  (trx--update-mode-line))

(defun trx--update-mode-line ()
  "Update `mode-name' with torrent count and total speeds."
  (when (and trx-torrent-vector (eq major-mode 'trx-mode))
    (let ((down 0) (up 0) (active 0) (total (length trx-torrent-vector)))
      (cl-loop for torrent across trx-torrent-vector do
               (let-alist torrent
                 (cl-incf down .rateDownload)
                 (cl-incf up .rateUpload)
                 (unless (zerop .status) (cl-incf active))))
      (setq mode-name
            (format "Trx %d/%d \u2193%d \u2191%d"
                    active total (trx-rate down) (trx-rate up))))))

(defun trx-draw-files (id)
  (let* ((arguments `(:ids ,id :fields ,trx-draw-files-keys))
         (response (trx-request "torrent-get" arguments)))
    (setq trx-torrent-vector (trx-torrents response)))
  (trx--set-default-directory)
  (let* ((files (trx-files-index (elt trx-torrent-vector 0)))
         (prefix (trx-files-prefix files)))
    (trx-do-entries files
      (propertize (format "%d%%" (trx-percent .bytesCompleted .length))
                  'font-lock-face 'trx-torrent-size)
      (propertize (symbol-name (car (rassq .priority trx-priority-alist)))
                  'font-lock-face 'trx-file-priority)
      (if (zerop .wanted) "no" "yes")
      (propertize (trx-size .length) 'font-lock-face 'trx-torrent-size)
      (propertize (if prefix (string-remove-prefix prefix .name) .name)
                  'font-lock-face 'trx-file-name 'trx-name t)))
  (tabulated-list-print)
  (trx--apply-fades))

(defmacro trx-insert-each-when (&rest body)
  "Insert each non-nil form in BODY sequentially on its own line."
  (declare (indent 0) (debug t))
  (let ((tmp (make-symbol "tmp")))
    (cl-loop for form in body
             collect `(when (setq ,tmp ,form) (insert ,tmp "\n")) into res
             finally return `(let (,tmp) ,@res))))

(defun trx-draw-info (id)
  (let* ((arguments `(:ids ,id :fields ,trx-draw-info-keys))
         (response (trx-request "torrent-get" arguments)))
    (setq trx-torrent-vector (trx-torrents response)))
  (trx--set-default-directory)
  (erase-buffer)
  (let-alist (elt trx-torrent-vector 0)
    (trx-insert-each-when
      (format "ID: %d" .id)
      (concat "Name: " .name)
      (concat "Hash: " id)
      (concat "Magnet: " (propertize .magnetLink 'font-lock-face 'link))
      (if (zerop (length .labels)) ""
        (concat "Labels: " (mapconcat #'identity .labels ", ") "\n"))
      (concat "Location: " (abbreviate-file-name .downloadDir))
      (let* ((percent (* 100 .percentDone))
             (fmt (if (zerop (mod percent 1)) "%d" "%.2f")))
        (concat "Percent done: " (format fmt percent) "%"))
      (format "Bandwidth priority: %s"
              (car (rassq .bandwidthPriority trx-priority-alist)))
      (format "Queue position: %d" .queuePosition)
      (concat "Speed: "
              (trx-format-limits
               .honorsSessionLimits .rateDownload .rateUpload
               .downloadLimit .uploadLimit .downloadLimited .uploadLimited))
      (trx-format-ratio .uploadRatio .seedRatioMode .seedRatioLimit)
      (pcase .error
        ((or 2 3) (concat "Error: " (propertize .errorString 'font-lock-face 'error)))
        (1 (concat "Warning: " (propertize .errorString 'font-lock-face 'warning))))
      (trx-format-peers .peers .peersFrom .peersConnected
                                 .peersGettingFromUs .peersSendingToUs)
      (concat "Date created:    " (trx-time .dateCreated))
      (concat "Date added:      " (trx-time .addedDate))
      (concat "Date finished:   " (trx-time .doneDate))
      (concat "Latest Activity: " (trx-time .activityDate) "\n")
      (trx-format-trackers .trackerStats)
      (concat "Wanted: " (trx-format-size .sizeWhenDone))
      (concat "Downloaded: " (trx-format-size .downloadedEver))
      (concat "Verified: " (trx-format-size .haveValid))
      (unless (zerop .corruptEver)
        (concat "Corrupt: " (trx-format-size .corruptEver)))
      (concat "Total size: " (trx-format-size .totalSize))
      (trx-format-pieces-internal .pieces .pieceCount .pieceSize))))

(defun trx-draw-peers (id)
  (let* ((arguments `(:ids ,id :fields ["peers"]))
         (response (trx-request "torrent-get" arguments)))
    (setq trx-torrent-vector (trx-torrents response)))
  (trx-do-entries (cdr (assq 'peers (elt trx-torrent-vector 0)))
    (propertize .address 'font-lock-face 'trx-peer-address)
    .flagStr
    (propertize (format "%d%%" (trx-percent .progress 1.0))
                'font-lock-face 'trx-torrent-size)
    (propertize (format "%d" (trx-rate .rateToClient))
                'font-lock-face 'trx-torrent-download)
    (propertize (format "%d" (trx-rate .rateToPeer))
                'font-lock-face 'trx-torrent-upload)
    (propertize .clientName 'font-lock-face 'trx-peer-client)
    (propertize (or (trx-geoip-retrieve .address) "")
                'font-lock-face 'trx-peer-location))
  (tabulated-list-print)
  (trx--apply-fades))

(defun trx--set-default-directory ()
  "Set `default-directory' from the torrent's download directory."
  (let ((dir (cdr (assq 'downloadDir (elt trx-torrent-vector 0)))))
    (when (and dir (file-directory-p dir))
      (setq default-directory (file-name-as-directory dir)))))

(defmacro define-trx-refresher (name)
  "Define a function `trx-refresh-NAME' that refreshes a context buffer.
The defined function takes no arguments and expects
`trx-draw-NAME' to exist.
Window position, point, and mark are restored, and the timer
object `trx-timer' is run."
  (declare (indent 1) (debug (symbolp)))
  (let ((thing (symbol-name name)))
    `(defun ,(intern (concat "trx-refresh-" thing)) (_arg _noconfirm)
       (trx-with-saved-state
         (run-hooks 'before-revert-hook)
         (with-silent-modifications
           (,(intern (concat "trx-draw-" thing)) trx-torrent-id))
         (run-hooks 'after-revert-hook))
       (trx-timer-check))))

(define-trx-refresher torrents)
(define-trx-refresher files)
(define-trx-refresher info)
(define-trx-refresher peers)

(defmacro trx-context (mode)
  "Switch to a context buffer of major mode MODE.
Uses per-torrent buffer names so multiple torrents can be viewed
simultaneously."
  (declare (debug (symbolp)))
  (cl-assert (string-suffix-p "-mode" (symbol-name mode)))
  (let ((base (string-remove-suffix "-mode" (symbol-name mode))))
    `(let* ((id (or trx-torrent-id
                    (cdr (assq 'hashString (tabulated-list-get-id)))))
            (torrent-name (cdr (assq 'name (tabulated-list-get-id))))
            (buf-name (format "*%s: %s*"
                              ,base
                              (or torrent-name (substring id 0 12)))))
       (if (not id) (user-error "No torrent selected")
         (let ((buffer (or (get-buffer buf-name)
                           (generate-new-buffer buf-name))))
           (trx-turtle-poll)
           (with-current-buffer buffer
             (unless (eq major-mode ',mode)
               (funcall #',mode))
             (unless (equal trx-torrent-id id)
               (setq trx-torrent-id id)
               (setq trx-marked-ids nil))
             (revert-buffer)
             (goto-char (point-min)))
           (pop-to-buffer-same-window buffer))))))

(defun trx-kill-torrent-buffers ()
  "Kill all per-torrent detail buffers."
  (interactive)
  (let ((n 0))
    (dolist (buf (buffer-list))
      (when (string-match-p "\\`\\*trx-\\(files\\|info\\|peers\\): " (buffer-name buf))
        (kill-buffer buf)
        (cl-incf n)))
    (message "Killed %d torrent buffer%s" n (if (= n 1) "" "s"))))

(defun trx-print-torrent (id cols)
  "Insert a torrent entry with fade truncation and mark support.
ID is a Lisp object identifying the entry to print, and COLS is
a vector of column descriptors."
  (let ((beg (point))
        (x (max tabulated-list-padding 0))
        (ncols (length tabulated-list-format))
        (inhibit-read-only t))
    (when (> tabulated-list-padding 0)
      (insert (make-string x ?\s)))
    (dotimes (n ncols)
      (let* ((fmt (aref tabulated-list-format n))
             (width (nth 1 fmt))
             (props (nthcdr 3 fmt))
             (right-align (plist-get props :right-align))
             (pad-right (or (plist-get props :pad-right) 1))
             (label (aref cols n))
             (label-width (string-width label)))
        (let ((face (or (get-text-property 0 'font-lock-face label)
                       (get-text-property 0 'face label))))
          (cond
           ((= n (1- ncols))
            (insert (trx--truncate
                     label (- (window-width) x 1) face)))
           (right-align
            (let ((shift (- width label-width)))
              (when (> shift 0) (insert (make-string shift ?\s)))
              (insert label)
              (insert (make-string pad-right ?\s))
              (cl-incf x (+ width pad-right))))
           (t
            (insert (trx--truncate label width face))
            (let ((pad (- width (min label-width width))))
              (insert (make-string (+ pad pad-right) ?\s)))
            (cl-incf x (+ width pad-right)))))))
    (insert ?\n)
    (put-text-property beg (point) 'tabulated-list-id id))
  (trx--print-torrent-mark id))

(defun trx--print-torrent-mark (id)
  "Put the mark tag on the current line if ID is marked."
  (let ((key (cl-case major-mode
               (trx-mode 'hashString)
               (trx-files-mode 'index))))
    (when key
      (let ((item-id (cdr (assq key id))))
        (when (member item-id trx-marked-ids)
          (save-excursion
            (forward-line -1)
            (tabulated-list-put-tag ">")))))))

;; Major mode definitions

(defmacro define-trx-predicate (name test &rest body)
  "Define trx-NAME as a function.
The function is to be used as a `sort' predicate for `tabulated-list-format'.
The definition is (lambda (a b) (TEST ...)) where the body
is constructed from TEST, BODY and the `tabulated-list-id' tagged as `<>'."
  (declare (indent 2) (debug (symbolp function-form body)))
  (let ((a (make-symbol "a"))
        (b (make-symbol "b")))
    (cl-labels
        ((cut (form x)
           (cond
            ((eq form '<>) (list 'car x))
            ((atom form) form)
            ((or (listp form) (null form))
             (mapcar (lambda (subexp) (cut subexp x)) form)))))
      `(defun ,(intern (concat "trx-" (symbol-name name))) (,a ,b)
         (,test ,(cut (macroexp-progn body) a)
                ,(cut (macroexp-progn body) b))))))

(define-trx-predicate download>? > (cdr (assq 'rateToClient <>)))
(define-trx-predicate upload>? > (cdr (assq 'rateToPeer <>)))
(define-trx-predicate size>? > (cdr (assq 'length <>)))
(define-trx-predicate size-when-done>? > (cdr (assq 'sizeWhenDone <>)))
(define-trx-predicate percent-done>? > (cdr (assq 'percentDone <>)))
(define-trx-predicate ratio>? > (cdr (assq 'uploadRatio <>)))
(define-trx-predicate progress>? > (cdr (assq 'progress <>)))
(define-trx-predicate file-want? > (cdr (assq 'wanted <>)))
(define-trx-predicate added>? > (cdr (assq 'addedDate <>)))

(define-trx-predicate eta>=? >=
  (let-alist <>
    (if (>= .eta 0) .eta
      (- 1.0 .percentDone))))

(define-trx-predicate file-have>? >
  (let-alist <>
    (/ (* 1.0 .bytesCompleted) .length)))

(defvar trx-peers-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "i" 'trx-info)
    map)
  "Keymap used in `trx-peers-mode' buffers.")

(easy-menu-define trx-peers-mode-menu trx-peers-mode-map
  "Menu used in `trx-peers-mode' buffers."
  '("Trx-Peers"
    ["View Torrent Files" trx-files]
    ["View Torrent Info" trx-info]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" quit-window]))

(define-derived-mode trx-peers-mode tabulated-list-mode "Trx-Peers"
  "Major mode for viewing peer information.
See the \"--peer-info\" option in transmission-remote(1) or
https://github.com/transmission/transmission/blob/main/docs/Peer-Status-Text.md
for explanation of the peer flags."
  :group 'trx
  (setq-local line-move-visual nil)
  (setq tabulated-list-format
        [("Address" 15 nil)
         ("Flags" 6 t)
         ("Has" 4 trx-progress>? :right-align t)
         ("Down" 4 trx-download>? :right-align t)
         ("Up" 3 trx-upload>? :right-align t :pad-right 2)
         ("Client" 20 t)
         ("Location" 0 t)])
  (setq tabulated-list-printer #'trx-print-torrent)
  (tabulated-list-init-header)
  (add-hook 'post-command-hook #'trx-timer-check nil t)
  (setq-local revert-buffer-function #'trx-refresh-peers))

(defun trx-peers ()
  "Open a `trx-peers-mode' buffer for torrent at point."
  (interactive)
  (trx-context trx-peers-mode))

(defvar trx-info-font-lock-keywords
  (eval-when-compile
    `((,(rx bol (group (*? nonl) ":") (* blank) (group (* nonl)) eol)
       (1 'font-lock-type-face)
       (2 'font-lock-keyword-face))))
  "Default expressions to highlight in `trx-info-mode' buffers.")

(defvar trx-info-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'trx-files)
    (define-key map "p" 'previous-line)
    (define-key map "n" 'next-line)
    (define-key map "a" 'trx-trackers-add)
    (define-key map "c" 'trx-copy-magnet)
    (define-key map "d" 'trx-set-torrent-download)
    (define-key map "e" 'trx-peers)
    (define-key map "L" 'trx-label)
    (define-key map "l" 'trx-set-torrent-ratio)
    (define-key map "r" 'trx-trackers-remove)
    (define-key map "u" 'trx-set-torrent-upload)
    (define-key map "y" 'trx-set-bandwidth-priority)
    map)
  "Keymap used in `trx-info-mode' buffers.")

(easy-menu-define trx-info-mode-menu trx-info-mode-map
  "Menu used in `trx-info-mode' buffers."
  '("Trx-Info"
    ["Add Tracker URLs" trx-trackers-add]
    ["Remove Trackers" trx-trackers-remove]
    ["Replace Tracker" trx-trackers-replace]
    ["Copy Magnet Link" trx-copy-magnet]
    ["Move Torrent" trx-move]
    ["Reannounce Torrent" trx-reannounce]
    ["Set Bandwidth Priority" trx-set-bandwidth-priority]
    ("Set Torrent Limits"
     ["Honor Session Speed Limits" trx-toggle-limits
      :help "Toggle whether torrent honors session limits."
      :style toggle :selected (trx-torrent-honors-speed-limits-p)]
     ["Set Torrent Download Limit" trx-set-torrent-download]
     ["Set Torrent Upload Limit" trx-set-torrent-upload]
     ["Set Torrent Seed Ratio Limit" trx-set-torrent-ratio])
    ("Set Torrent Queue Position"
     ["Move To Top" trx-queue-move-top]
     ["Move To Bottom" trx-queue-move-bottom]
     ["Move Up" trx-queue-move-up]
     ["Move Down" trx-queue-move-down])
    ["Set Torrent Labels" trx-label]
    ["Verify Torrent" trx-verify]
    "--"
    ["View Torrent Files" trx-files]
    ["View Torrent Peers" trx-peers]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" quit-window]))

(define-derived-mode trx-info-mode special-mode "Trx-Info"
  "Major mode for viewing and manipulating torrent attributes."
  :group 'trx
  (setq buffer-undo-list t)
  (setq font-lock-defaults '(trx-info-font-lock-keywords t))
  (add-hook 'post-command-hook #'trx-timer-check nil t)
  (setq-local revert-buffer-function #'trx-refresh-info))

(defun trx-info ()
  "Open a `trx-info-mode' buffer for torrent at point."
  (interactive)
  (trx-context trx-info-mode))

(defvar trx-files-font-lock-keywords
  '(("^[>]" (".+" (trx-move-to-file-name) nil (0 'warning))))
  "Default expressions to highlight in `trx-files-mode'.")

(defvar trx-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'trx-find-file)
    (define-key map "o" 'trx-find-file-other-window)
    (define-key map (kbd "C-o") 'trx-display-file)
    (define-key map "^" 'quit-window)
    (define-key map "!" 'trx-files-command)
    (define-key map "&" 'trx-files-command)
    (define-key map "X" 'trx-files-command)
    (define-key map "W" 'trx-browse-url-of-file)
    (define-key map "C" 'trx-copy-file)
    (define-key map "R" 'trx-rename-path)
    (define-key map "d" 'trx-dired-file)
    (define-key map "e" 'trx-peers)
    (define-key map "i" 'trx-info)
    (define-key map "m" 'trx-toggle-mark)
    (define-key map "t" 'trx-invert-marks)
    (define-key map "u" 'trx-files-unwant)
    (define-key map "U" 'trx-unmark-all)
    (define-key map "v" 'trx-view-file)
    (define-key map "w" 'trx-files-want)
    (define-key map "y" 'trx-files-priority)
    map)
  "Keymap used in `trx-files-mode' buffers.")

(easy-menu-define trx-files-mode-menu trx-files-mode-map
  "Menu used in `trx-files-mode' buffers."
  '("Trx-Files"
    ["Run Command On File" trx-files-command]
    ["Visit File" trx-find-file
     "Switch to a read-only buffer visiting file at point"]
    ["Visit File In Other Window" trx-find-file-other-window]
    ["Display File" trx-display-file
     "Display a read-only buffer visiting file at point"]
    ["Visit File In View Mode" trx-view-file]
    ["Open File In WWW Browser" trx-browse-url-of-file]
    ["Show File In DirEd" trx-dired-file]
    ["Copy File Name" trx-copy-filename-as-kill]
    ["Rename File" trx-rename-path]
    "--"
    ["Unwant Files" trx-files-unwant
     :help "Tell Trx not to download files at point or in region"]
    ["Want Files" trx-files-want
     :help "Tell Trx to download files at point or in region"]
    ["Set Files' Bandwidth Priority" trx-files-priority]
    "--"
    ["Toggle Mark" trx-toggle-mark]
    ["Unmark All" trx-unmark-all]
    ["Invert Marks" trx-invert-marks]
    "--"
    ["View Torrent Info" trx-info]
    ["View Torrent Peers" trx-peers]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" quit-window]))

(define-derived-mode trx-files-mode tabulated-list-mode "Trx-Files"
  "Major mode for a torrent's file list."
  :group 'trx
  (setq-local line-move-visual nil)
  (setq tabulated-list-format
        [("Have" 4 trx-file-have>? :right-align t)
         ("Priority" 8 t)
         ("Want" 4 trx-file-want? :right-align t)
         ("Size" 9 trx-size>? :right-align t :trx-size t)
         ("Name" 0 t)])
  (setq tabulated-list-padding 1)
  (trx-tabulated-list-format)
  (setq-local file-name-at-point-functions '(trx-files-file-at-point))
  (setq tabulated-list-printer #'trx-print-torrent)
  (setq-local revert-buffer-function #'trx-refresh-files)
  (setq-local font-lock-defaults '(trx-files-font-lock-keywords t))
  (add-hook 'post-command-hook #'trx-timer-check nil t)
  (add-hook 'before-revert-hook #'trx-tabulated-list-format nil t))

(defun trx-files ()
  "Open a `trx-files-mode' buffer for torrent at point."
  (interactive)
  (trx-context trx-files-mode))

(defvar trx-font-lock-keywords
  '(("^[>]" (trx-file-name-matcher nil nil (0 'warning))))
  "Default expressions to highlight in `trx-mode'.")

(defvar trx-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry #x221e "w" table)
    table)
  "Syntax table used in `trx-mode' buffers.")

(defvar trx-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "!" 'trx-files)
    (define-key map (kbd "RET") 'trx-files)
    (define-key map "a" 'trx-add)
    (define-key map "d" 'trx-set-download)
    (define-key map "e" 'trx-peers)
    (define-key map "i" 'trx-info)
    (define-key map "k" 'trx-trackers-add)
    (define-key map "L" 'trx-label)
    (define-key map "l" 'trx-set-ratio)
    (define-key map "m" 'trx-toggle-mark)
    (define-key map "r" 'trx-remove)
    (define-key map "D" 'trx-delete)
    (define-key map "s" 'trx-toggle)
    (define-key map "t" 'trx-invert-marks)
    (define-key map "u" 'trx-set-upload)
    (define-key map "v" 'trx-verify)
    (define-key map "q" 'trx-quit)
    (define-key map "y" 'trx-set-bandwidth-priority)
    (define-key map "U" 'trx-unmark-all)
    (define-key map "/" 'trx-filter)
    (define-key map "\\" 'trx-filter-clear)
    map)
  "Keymap used in `trx-mode' buffers.")

(easy-menu-define trx-mode-menu trx-mode-map
  "Menu used in `trx-mode' buffers."
  '("Trx"
    ["Add Torrent" trx-add]
    ["Start/Stop Torrent" trx-toggle
     :help "Toggle pause on torrents at point or in region"]
    ["Set Bandwidth Priority" trx-set-bandwidth-priority]
    ["Add Tracker URLs" trx-trackers-add]
    ("Set Global/Session Limits"
     ["Set Global Download Limit" trx-set-download]
     ["Set Global Upload Limit" trx-set-upload]
     ["Set Global Seed Ratio Limit" trx-set-ratio])
    ("Set Torrent Limits"
     ["Toggle Torrent Speed Limits" trx-toggle-limits
      :help "Toggle whether torrent honors session limits."]
     ["Set Torrent Download Limit" trx-set-torrent-download]
     ["Set Torrent Upload Limit" trx-set-torrent-upload]
     ["Set Torrent Seed Ratio Limit" trx-set-torrent-ratio])
    ["Move Torrent" trx-move]
    ["Remove Torrent" trx-remove]
    ["Delete Torrent" trx-delete
     :help "Delete torrent contents from disk."]
    ["Reannounce Torrent" trx-reannounce]
    ("Set Torrent Queue Position"
     ["Move To Top" trx-queue-move-top]
     ["Move To Bottom" trx-queue-move-bottom]
     ["Move Up" trx-queue-move-up]
     ["Move Down" trx-queue-move-down])
    ["Set Torrent Labels" trx-label]
    ["Verify Torrent" trx-verify]
    "--"
    ["Toggle Mark" trx-toggle-mark]
    ["Unmark All" trx-unmark-all]
    ["Invert Marks" trx-invert-marks
     :help "Toggle mark on all items"]
    "--"
    ["Query Free Space" trx-free]
    ["Session Statistics" trx-stats]
    ("Turtle Mode" :help "Set and schedule alternative speed limits"
     ["Turtle Mode Status" trx-turtle-status]
     ["Toggle Turtle Mode" trx-turtle-mode]
     ["Set Active Days" trx-turtle-set-days]
     ["Set Active Time Span" trx-turtle-set-times]
     ["Set Turtle Speed Limits" trx-turtle-set-speeds])
    ("Daemon"
     ["Start Daemon" trx-daemon-start]
     ["Stop Daemon" trx-daemon-stop])
    "--"
    ["Filter Torrents" trx-filter]
    ["Clear Filter" trx-filter-clear]
    "--"
    ["View Torrent Files" trx-files]
    ["View Torrent Info" trx-info]
    ["View Torrent Peers" trx-peers]
    "--"
    ["Refresh" revert-buffer]
    ["Quit" trx-quit]))

(define-derived-mode trx-mode tabulated-list-mode "Trx"
  "Major mode for the list of torrents in a Transmission session.
See https://github.com/transmission/transmission for more information about
Transmission."
  :group 'trx
  (setq-local line-move-visual nil)
  (setq-local trx-filter-active nil)
  (setq tabulated-list-format
        [("ETA" 4 trx-eta>=? :right-align t)
         ("Size" 9 trx-size-when-done>?
          :right-align t :trx-size t)
         ("Have" 4 trx-percent-done>? :right-align t)
         ("Down" 4 nil :right-align t)
         ("Up" 3 nil :right-align t)
         ("Ratio" 5 trx-ratio>? :right-align t)
         ("Status" 11 t)
         ("Added" 6 trx-added>? :right-align t)
         ("Name" 0 t)])
  (setq tabulated-list-padding 1)
  (trx-tabulated-list-format)
  (setq tabulated-list-printer #'trx-print-torrent)
  (setq-local revert-buffer-function #'trx-refresh-torrents)
  (setq-local font-lock-defaults '(trx-font-lock-keywords t))
  (add-hook 'post-command-hook #'trx-timer-check nil t)
  (add-hook 'before-revert-hook #'trx-tabulated-list-format nil t))

;;;###autoload
(defun trx ()
  "Open a `trx-mode' buffer."
  (interactive)
  (let* ((name "*trx*")
         (buffer (or (get-buffer name)
                     (generate-new-buffer name))))
    (trx-turtle-poll)
    (trx-refresh-session-cache)
    (unless (eq buffer (current-buffer))
      (with-current-buffer buffer
        (unless (eq major-mode 'trx-mode)
          (condition-case e
              (progn
                (trx-mode)
                (revert-buffer)
                (goto-char (point-min)))
            (error
             (kill-buffer buffer)
             (signal (car e) (cdr e))))))
      (pop-to-buffer buffer))))

(provide 'trx)

;;; trx.el ends here
