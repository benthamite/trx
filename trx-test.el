;;; trx-test.el --- Tests for trx.el -*- lexical-binding: t -*-

;; Copyright (C) 2026 Pablo Stafforini

;;; Commentary:

;; Comprehensive ERT test suite for trx.el, the Emacs interface to
;; Transmission.  Tests are organized by function category: pure
;; utilities, formatting, data processing, HTTP/RPC parsing, and
;; filtering.

;;; Code:

(require 'ert)
(require 'trx)
(require 'cl-lib)


;;;; Pure utility functions

;;;;; trx-percent

(ert-deftest trx-percent-normal ()
  "Percentage of a portion of a total."
  (should (= 50.0 (trx-percent 50 100)))
  (should (= 100.0 (trx-percent 100 100)))
  (should (= 0.0 (trx-percent 0 100))))

(ert-deftest trx-percent-zero-total ()
  "Zero total returns 0, not a division error."
  (should (= 0 (trx-percent 50 0)))
  (should (= 0 (trx-percent 0 0))))

(ert-deftest trx-percent-fractional ()
  "Fractional percentages."
  (should (< (abs (- (trx-percent 1 3) 33.333)) 0.1)))

;;;;; trx-slice

(ert-deftest trx-slice-even-split ()
  "Slice a string into equal parts."
  (should (equal '("ab" "cd" "ef") (trx-slice "abcdef" 3))))

(ert-deftest trx-slice-uneven-split ()
  "Slice where length is not divisible by K."
  (let ((result (trx-slice "abcde" 3)))
    (should (= 3 (length result)))
    (should (= 5 (apply #'+ (mapcar #'length result))))))

(ert-deftest trx-slice-k-greater-than-length ()
  "K larger than string length returns individual characters."
  (should (equal '("a" "b" "c") (trx-slice "abc" 10))))

(ert-deftest trx-slice-single-slice ()
  "K=1 returns the whole string."
  (should (equal '("hello") (trx-slice "hello" 1))))

(ert-deftest trx-slice-empty-string ()
  "Empty string returns empty list."
  (should (null (trx-slice "" 3))))

(ert-deftest trx-slice-preserves-content ()
  "Slicing and rejoining preserves the original string."
  (let ((str "the quick brown fox"))
    (dolist (k '(1 2 3 5 7 19 50))
      (should (equal str (apply #'concat (trx-slice str k)))))))

;;;;; trx-eta

(ert-deftest trx-eta-done ()
  "Completed torrent shows Done."
  (should (equal "Done" (trx-eta 0 1))))

(ert-deftest trx-eta-infinity ()
  "Zero seconds with incomplete percent shows infinity."
  (let ((result (trx-eta 0 0.5)))
    (should (or (equal result "\u221e") (equal result "Inf")))))

(ert-deftest trx-eta-negative-incomplete ()
  "Negative seconds with incomplete percent shows infinity."
  (let ((result (trx-eta -1 0.5)))
    (should (or (equal result "\u221e") (equal result "Inf")))))

(ert-deftest trx-eta-seconds ()
  "Small values show seconds."
  (should (equal "30s" (trx-eta 30 0.5))))

(ert-deftest trx-eta-minutes ()
  "Values in minute range."
  (should (equal "5m" (trx-eta 300 0.5))))

(ert-deftest trx-eta-hours ()
  "Values in hour range."
  (should (equal "2h" (trx-eta 7200 0.5))))

(ert-deftest trx-eta-days ()
  "Values in day range."
  (should (equal "3d" (trx-eta (* 3 86400) 0.5))))

(ert-deftest trx-eta-months ()
  "Values in month range."
  (should (equal "2mo" (trx-eta (* 60 86400) 0.5))))

(ert-deftest trx-eta-years ()
  "Values in year range."
  (should (equal "2y" (trx-eta (* 730 86400) 0.5))))

(ert-deftest trx-eta-boundary-minute ()
  "Exactly 60 seconds is 1 minute."
  (should (equal "1m" (trx-eta 60 0.5))))

(ert-deftest trx-eta-boundary-hour ()
  "Exactly 3600 seconds is 1 hour."
  (should (equal "1h" (trx-eta 3600 0.5))))

(ert-deftest trx-eta-boundary-day ()
  "Exactly 86400 seconds is 1 day."
  (should (equal "1d" (trx-eta 86400 0.5))))

;;;;; trx-rate

(ert-deftest trx-rate-default-units ()
  "Default (SI) units divide by 1000."
  (let ((trx-units nil))
    (should (= 1 (trx-rate 1000)))))

(ert-deftest trx-rate-iec-units ()
  "IEC units divide by 1024."
  (let ((trx-units 'iec))
    (should (= 1 (trx-rate 1024)))))

(ert-deftest trx-rate-si-units ()
  "SI units divide by 1000."
  (let ((trx-units 'si))
    (should (= 1 (trx-rate 1000)))))

;;;;; trx-hamming-weight

(ert-deftest trx-hamming-weight-zero ()
  "Hamming weight of 0 is 0."
  (should (= 0 (trx-hamming-weight 0))))

(ert-deftest trx-hamming-weight-powers-of-two ()
  "Powers of two have Hamming weight 1."
  (dolist (n '(1 2 4 8 16 32 64 128))
    (should (= 1 (trx-hamming-weight n)))))

(ert-deftest trx-hamming-weight-all-bits ()
  "0xFF has Hamming weight 8."
  (should (= 8 (trx-hamming-weight #xff))))

(ert-deftest trx-hamming-weight-mixed ()
  "Various byte values."
  (should (= 4 (trx-hamming-weight #x55)))
  (should (= 4 (trx-hamming-weight #xaa)))
  (should (= 3 (trx-hamming-weight #x07)))
  (should (= 7 (trx-hamming-weight #xdf))))

(ert-deftest trx-hamming-weight-single-bit ()
  "Every single-bit value returns 1."
  (dotimes (i 8)
    (should (= 1 (trx-hamming-weight (ash 1 i))))))

;;;;; trx-count-bits

(ert-deftest trx-count-bits-empty ()
  "Empty byte array has 0 bits."
  (should (= 0 (trx-count-bits ""))))

(ert-deftest trx-count-bits-all-ones ()
  "All-ones bytes."
  (should (= 8 (trx-count-bits (string #xff))))
  (should (= 16 (trx-count-bits (string #xff #xff)))))

(ert-deftest trx-count-bits-mixed ()
  "Mixed byte values."
  (should (= 5 (trx-count-bits (string #x07 #xc0)))))

;;;;; trx-byte->string

(ert-deftest trx-byte->string-zero ()
  "Zero byte is all zeros."
  (should (equal "00000000" (trx-byte->string 0))))

(ert-deftest trx-byte->string-max ()
  "0xFF is all ones."
  (should (equal "11111111" (trx-byte->string #xff))))

(ert-deftest trx-byte->string-one ()
  "Byte 1 has only the last bit set."
  (should (equal "00000001" (trx-byte->string 1))))

(ert-deftest trx-byte->string-128 ()
  "Byte 128 has only the first bit set."
  (should (equal "10000000" (trx-byte->string 128))))

;;;;; trx-ratio->glyph

(ert-deftest trx-ratio->glyph-zero ()
  "Zero ratio is a space."
  (should (equal " " (trx-ratio->glyph 0))))

(ert-deftest trx-ratio->glyph-low ()
  "Low ratio is light shade."
  (should (equal "\u2591" (trx-ratio->glyph 0.1))))

(ert-deftest trx-ratio->glyph-medium ()
  "Medium ratio is medium shade."
  (should (equal "\u2592" (trx-ratio->glyph 0.5))))

(ert-deftest trx-ratio->glyph-high ()
  "High ratio is dark shade."
  (should (equal "\u2593" (trx-ratio->glyph 0.9))))

(ert-deftest trx-ratio->glyph-full ()
  "Full ratio is full block."
  (should (equal "\u2588" (trx-ratio->glyph 1))))

;;;;; trx-group-digits

(ert-deftest trx-group-digits-small ()
  "Numbers below 10000 are not grouped."
  (let ((trx-digit-delimiter ","))
    (should (equal "9999" (trx-group-digits 9999)))
    (should (equal "0" (trx-group-digits 0)))
    (should (equal "100" (trx-group-digits 100)))))

(ert-deftest trx-group-digits-large ()
  "Numbers 10000+ are grouped with delimiter."
  (let ((trx-digit-delimiter ","))
    (should (equal "10,000" (trx-group-digits 10000)))
    (should (equal "1,000,000" (trx-group-digits 1000000)))))

(ert-deftest trx-group-digits-dot-delimiter ()
  "Dot delimiter works."
  (let ((trx-digit-delimiter "."))
    (should (equal "10.000" (trx-group-digits 10000)))))

;;;;; trx-plural

(ert-deftest trx-plural-singular ()
  "Singular form for count 1."
  (let ((trx-digit-delimiter ","))
    (should (equal "1 file" (trx-plural 1 "file")))))

(ert-deftest trx-plural-zero ()
  "Plural form for count 0."
  (let ((trx-digit-delimiter ","))
    (should (equal "0 files" (trx-plural 0 "file")))))

(ert-deftest trx-plural-many ()
  "Plural form for count > 1."
  (let ((trx-digit-delimiter ","))
    (should (equal "5 peers" (trx-plural 5 "peer")))))

(ert-deftest trx-plural-negative-one ()
  "Count -1 is treated as 0."
  (let ((trx-digit-delimiter ","))
    (should (equal "0 files" (trx-plural -1 "file")))))

(ert-deftest trx-plural-large-number ()
  "Large numbers are grouped."
  (let ((trx-digit-delimiter ","))
    (should (equal "10,000 bytes" (trx-plural 10000 "byte")))))

;;;;; trx-btih-p

(ert-deftest trx-btih-p-valid ()
  "Valid 40-char hex info hash."
  (let ((hash "0123456789abcdef0123456789abcdef01234567"))
    (should (equal hash (trx-btih-p hash)))))

(ert-deftest trx-btih-p-uppercase ()
  "Uppercase hex is also valid."
  (let ((hash "0123456789ABCDEF0123456789ABCDEF01234567"))
    (should (equal hash (trx-btih-p hash)))))

(ert-deftest trx-btih-p-too-short ()
  "39 characters is not valid."
  (should-not (trx-btih-p "0123456789abcdef0123456789abcdef0123456")))

(ert-deftest trx-btih-p-too-long ()
  "41 characters is not valid."
  (should-not (trx-btih-p "0123456789abcdef0123456789abcdef012345678")))

(ert-deftest trx-btih-p-non-hex ()
  "Non-hex characters fail."
  (should-not (trx-btih-p "0123456789ghijkl0123456789abcdef01234567")))

(ert-deftest trx-btih-p-nil ()
  "Nil input returns nil."
  (should-not (trx-btih-p nil)))

(ert-deftest trx-btih-p-url ()
  "A URL is not an info hash."
  (should-not (trx-btih-p "http://example.com/file.torrent")))

;;;;; trx-directory-name-p

(ert-deftest trx-directory-name-p-slash ()
  "Trailing slash is a directory."
  (should (trx-directory-name-p "/some/path/")))

(ert-deftest trx-directory-name-p-no-slash ()
  "No trailing slash is not a directory."
  (should-not (trx-directory-name-p "/some/path/file.txt")))

(ert-deftest trx-directory-name-p-empty ()
  "Empty string is not a directory."
  (should-not (trx-directory-name-p "")))

(ert-deftest trx-directory-name-p-root ()
  "Root directory."
  (should (trx-directory-name-p "/")))

;;;;; trx-levi-civita

(ert-deftest trx-levi-civita-even-permutation ()
  "Even permutations return 1."
  (should (= 1 (trx-levi-civita 1 2 3)))
  (should (= 1 (trx-levi-civita 2 3 1)))
  (should (= 1 (trx-levi-civita 3 1 2))))

(ert-deftest trx-levi-civita-odd-permutation ()
  "Odd permutations return -1."
  (should (= -1 (trx-levi-civita 3 2 1)))
  (should (= -1 (trx-levi-civita 1 3 2)))
  (should (= -1 (trx-levi-civita 2 1 3))))

(ert-deftest trx-levi-civita-repeated-index ()
  "Repeated indices return 0."
  (should (= 0 (trx-levi-civita 1 1 2)))
  (should (= 0 (trx-levi-civita 1 2 2)))
  (should (= 0 (trx-levi-civita 3 3 3))))

;;;;; trx-n->days

(ert-deftest trx-n->days-single ()
  "Single day flags."
  (should (equal '(sun) (trx-n->days 1)))
  (should (equal '(mon) (trx-n->days 2)))
  (should (equal '(sat) (trx-n->days 64))))

(ert-deftest trx-n->days-weekday ()
  "Weekday composite."
  (should (equal '(weekday)
                 (trx-n->days (cdr (assq 'weekday trx-schedules))))))

(ert-deftest trx-n->days-weekend ()
  "Weekend composite."
  (should (equal '(weekend)
                 (trx-n->days (cdr (assq 'weekend trx-schedules))))))

(ert-deftest trx-n->days-all ()
  "All days."
  (should (equal '(all)
                 (trx-n->days (cdr (assq 'all trx-schedules))))))

(ert-deftest trx-n->days-combination ()
  "Combination of individual days."
  (should (equal '(sun mon) (trx-n->days 3))))

;;;;; trx-format-minutes

(ert-deftest trx-format-minutes-format ()
  "Output matches HH:MM format."
  (should (string-match-p "\\`[0-2][0-9]:[0-5][0-9]\\'"
                          (trx-format-minutes 0)))
  (should (string-match-p "\\`[0-2][0-9]:[0-5][0-9]\\'"
                          (trx-format-minutes 720)))
  (should (string-match-p "\\`[0-2][0-9]:[0-5][0-9]\\'"
                          (trx-format-minutes 510))))

(ert-deftest trx-format-minutes-relative ()
  "Larger minute values produce later times."
  (let ((early (trx-format-minutes 0))
        (late (trx-format-minutes 720)))
    (should-not (equal early late))))

;;;;; trx-tracker-url-p

(ert-deftest trx-tracker-url-p-url ()
  "A URL is a tracker."
  (should (trx-tracker-url-p "http://tracker.example.com/announce")))

(ert-deftest trx-tracker-url-p-number ()
  "A number is not a tracker URL."
  (should-not (trx-tracker-url-p "42")))

(ert-deftest trx-tracker-url-p-blank ()
  "Blank string returns nil."
  (should-not (trx-tracker-url-p "   ")))

(ert-deftest trx-tracker-url-p-letter-start ()
  "String starting with a letter is a tracker."
  (should (trx-tracker-url-p "udp://tracker.opentrackr.org:1337")))

;;;;; trx-time

(ert-deftest trx-time-zero ()
  "Zero epoch is Never."
  (should (equal "Never" (trx-time 0))))

(ert-deftest trx-time-nonzero ()
  "Non-zero epoch produces a formatted string."
  (let ((trx-time-format "%Y")
        (trx-time-zone t))
    (should (equal "2000" (trx-time 946684800)))))

;;;;; trx-refs

(ert-deftest trx-refs-basic ()
  "Extract values for a key from a sequence of alists."
  (let ((seq (list '((name . "foo") (size . 100))
                   '((name . "bar") (size . 200)))))
    (should (equal '("foo" "bar") (trx-refs seq 'name)))
    (should (equal '(100 200) (trx-refs seq 'size)))))

(ert-deftest trx-refs-missing-key ()
  "Missing key returns nil for that element."
  (let ((seq (list '((name . "foo")) '((other . "bar")))))
    (should (equal '("foo" nil) (trx-refs seq 'name)))))

(ert-deftest trx-refs-empty ()
  "Empty sequence returns empty list."
  (should (null (trx-refs nil 'name))))


;;;; Formatting functions

;;;;; trx-format-ratio

(ert-deftest trx-format-ratio-normal ()
  "Normal ratio display."
  (should (string-match-p "1\\.500" (trx-format-ratio 1.5 0 nil))))

(ert-deftest trx-format-ratio-zero ()
  "Zero ratio displays 0.000."
  (should (string-match-p "0\\.000" (trx-format-ratio 0 0 nil))))

(ert-deftest trx-format-ratio-negative-two ()
  "Ratio -2 shows infinity."
  (let ((result (trx-format-ratio -2 0 nil)))
    (should (or (string-match-p "\u221e" result)
                (string-match-p "Inf" result)))))

(ert-deftest trx-format-ratio-negative-one ()
  "Ratio -1 shows n/a."
  (should (string-match-p "n/a" (trx-format-ratio -1 0 nil))))

(ert-deftest trx-format-ratio-session-mode ()
  "Mode 0 shows session limit."
  (should (string-match-p "session limit"
                          (trx-format-ratio 1.0 0 nil))))

(ert-deftest trx-format-ratio-torrent-mode ()
  "Mode 1 shows torrent-specific limit."
  (should (string-match-p "torrent-specific"
                          (trx-format-ratio 1.0 1 2.0))))

(ert-deftest trx-format-ratio-unlimited-mode ()
  "Mode 2 shows unlimited."
  (should (string-match-p "unlimited"
                          (trx-format-ratio 1.0 2 nil))))

;;;;; trx-format-speed-limit

(ert-deftest trx-format-speed-limit-unlimited ()
  "Unlimited speed shows just the rate."
  (let ((trx-units nil))
    (should (equal "5 kB/s"
                   (trx-format-speed-limit 5000 100 nil)))))

(ert-deftest trx-format-speed-limit-limited ()
  "Limited speed shows rate / limit."
  (let ((trx-units nil))
    (should (equal "5 / 100 kB/s"
                   (trx-format-speed-limit 5000 100 t)))))

;;;;; trx-format-limits

(ert-deftest trx-format-limits-basic ()
  "Combined limits string."
  (let ((trx-units nil))
    (let ((result (trx-format-limits nil 1000 2000 100 200 nil nil)))
      (should (string-match-p "down" result))
      (should (string-match-p "up" result)))))

(ert-deftest trx-format-limits-session-limited ()
  "Session limited appends note."
  (let ((trx-units nil))
    (should (string-match-p
             "session limited"
             (trx-format-limits t 1000 2000 100 200 nil nil)))))

;;;;; trx-format-status

(ert-deftest trx-format-status-stopped ()
  "Status 0 is stopped."
  (should (equal "stopped"
                 (substring-no-properties (trx-format-status 0 0 0)))))

(ert-deftest trx-format-status-downloading-active ()
  "Downloading with active download rate."
  (should (equal "downloading"
                 (substring-no-properties
                  (trx-format-status 4 0 100)))))

(ert-deftest trx-format-status-downloading-idle ()
  "Downloading with no activity shows idle."
  (should (equal "idle"
                 (substring-no-properties
                  (trx-format-status 4 0 0)))))

(ert-deftest trx-format-status-downloading-uploading ()
  "Downloading with upload but no download shows uploading."
  (should (equal "uploading"
                 (substring-no-properties
                  (trx-format-status 4 100 0)))))

(ert-deftest trx-format-status-seeding-active ()
  "Seeding with upload rate."
  (should (equal "seeding"
                 (substring-no-properties
                  (trx-format-status 6 100 0)))))

(ert-deftest trx-format-status-seeding-idle ()
  "Seeding with no upload shows idle."
  (should (equal "idle"
                 (substring-no-properties
                  (trx-format-status 6 0 0)))))

(ert-deftest trx-format-status-verifying ()
  "Status 2 is verifying."
  (should (equal "verifying"
                 (substring-no-properties
                  (trx-format-status 2 0 0)))))

(ert-deftest trx-format-status-wait-states ()
  "Wait states (1, 3, 5)."
  (dolist (status '(1 3 5))
    (should (equal (aref trx-status-names status)
                   (substring-no-properties
                    (trx-format-status status 0 0))))))

;;;;; trx-format-peers

(ert-deftest trx-format-peers-none-connected ()
  "No connected peers."
  (should (string-match-p "none connected"
                          (trx-format-peers [] nil 0 0 0))))

(ert-deftest trx-format-peers-with-connections ()
  "Connected peers display."
  (let ((peers (vector '((clientIsChoked . t)
                         (peerIsInterested . t))))
        (origins '((fromCache . 1) (fromDht . 0)
                   (fromIncoming . 0) (fromLpd . 0)
                   (fromLtep . 0) (fromPex . 0)
                   (fromTracker . 2))))
    (let ((result (trx-format-peers peers origins 3 1 1)))
      (should (string-match-p "3 connected" result))
      (should (string-match-p "uploading to 1" result))
      (should (string-match-p "downloading from 1" result))
      (should (string-match-p "Peer origins" result)))))

;;;;; trx-format-trackers

(ert-deftest trx-format-trackers-empty ()
  "Empty tracker array."
  (should (string-match-p "none" (trx-format-trackers []))))

;;;;; trx-format-size

(ert-deftest trx-format-size-basic ()
  "Size format includes both human-readable and byte count."
  (let ((trx-units nil)
        (trx-digit-delimiter ","))
    (let ((result (trx-format-size 1048576)))
      (should (string-match-p "bytes" result))
      (should (string-match-p "1,048,576" result)))))

;;;;; trx-format-pieces

(ert-deftest trx-format-pieces-basic ()
  "Pieces formatting produces a bitfield string."
  (let* ((bytes (unibyte-string #xff #x00))
         (pieces (base64-encode-string bytes))
         (result (trx-format-pieces pieces 16)))
    (should (stringp result))
    (should (string-match-p "1" result))
    (should (string-match-p "0" result))))

(ert-deftest trx-format-pieces-brief-basic ()
  "Brief pieces formatting returns a string."
  (let* ((bytes (apply #'unibyte-string (make-list 10 #xff)))
         (pieces (base64-encode-string bytes))
         (result (trx-format-pieces-brief pieces 80)))
    (should (stringp result))
    (should (> (length result) 0))))


;;;; Data processing functions

;;;;; trx-torrents

(ert-deftest trx-torrents-normal ()
  "Extract torrents array from response."
  (let ((response '((torrents . [((name . "foo"))
                                  ((name . "bar"))]))))
    (should (= 2 (length (trx-torrents response))))))

(ert-deftest trx-torrents-empty ()
  "Empty torrents array returns nil."
  (should-not (trx-torrents '((torrents . [])))))

(ert-deftest trx-torrents-missing ()
  "Missing torrents key returns nil."
  (should-not (trx-torrents '((other . "value")))))

;;;;; trx-unique-labels

(ert-deftest trx-unique-labels-basic ()
  "Extract unique labels from torrents."
  (let ((torrents (vector '((labels . ["linux" "iso"]))
                          '((labels . ["linux" "video"])))))
    (let ((result (trx-unique-labels torrents)))
      (should (member "linux" result))
      (should (member "iso" result))
      (should (member "video" result))
      (should (= 3 (length result))))))

(ert-deftest trx-unique-labels-no-labels ()
  "Torrents with empty label arrays."
  (let ((torrents (vector '((labels . []))
                          '((labels . [])))))
    (should-not (trx-unique-labels torrents))))

(ert-deftest trx-unique-labels-empty-vector ()
  "Empty torrent vector."
  (should-not (trx-unique-labels [])))

;;;;; trx-files-index

(ert-deftest trx-files-index-basic ()
  "Build file index from torrent data."
  (let* ((torrent '((files . [((name . "a.txt")
                                (length . 100)
                                (bytesCompleted . 50))
                               ((name . "b.txt")
                                (length . 200)
                                (bytesCompleted . 200))])
                    (wanted . [t :json-false])
                    (priorities . [0 1])))
         (result (trx-files-index torrent)))
    (should (= 2 (length result)))
    (let ((first (aref result 0)))
      (should (equal "a.txt" (cdr (assq 'name first))))
      (should (equal t (cdr (assq 'wanted first))))
      (should (= 0 (cdr (assq 'priority first))))
      (should (= 0 (cdr (assq 'index first)))))
    (let ((second (aref result 1)))
      (should (equal :json-false (cdr (assq 'wanted second))))
      (should (= 1 (cdr (assq 'priority second))))
      (should (= 1 (cdr (assq 'index second)))))))

;;;;; trx-files-prefix

(ert-deftest trx-files-prefix-common ()
  "Common directory prefix."
  (let ((files (vector '((name . "dir/a.txt"))
                       '((name . "dir/b.txt")))))
    (should (equal "dir/" (trx-files-prefix files)))))

(ert-deftest trx-files-prefix-no-common ()
  "No common prefix."
  (let ((files (vector '((name . "a.txt"))
                       '((name . "b.txt")))))
    (should (equal "" (trx-files-prefix files)))))

(ert-deftest trx-files-prefix-single-file ()
  "Single file returns empty prefix (no slash in name)."
  (let ((files (vector '((name . "file.txt")))))
    (should (equal "" (trx-files-prefix files)))))

(ert-deftest trx-files-prefix-nested ()
  "Nested common prefix."
  (let ((files (vector '((name . "a/b/c.txt"))
                       '((name . "a/b/d.txt")))))
    (should (equal "a/b/" (trx-files-prefix files)))))

(ert-deftest trx-files-prefix-empty ()
  "Empty file vector."
  (should-not (trx-files-prefix [])))

;;;;; trx-filter-apply

(ert-deftest trx-filter-apply-name ()
  "Filter by name matches case-insensitively via `case-fold-search'."
  (let ((torrents (vector '((name . "Ubuntu 24.04") (status . 6))
                          '((name . "Fedora 40") (status . 4))
                          '((name . "ubuntu server") (status . 0)))))
    (let ((result (trx-filter-apply torrents "Ubuntu")))
      (should (= 2 (length result))))))

(ert-deftest trx-filter-apply-status ()
  "Filter by status."
  (let ((torrents (vector '((name . "A") (status . 0))
                          '((name . "B") (status . 4))
                          '((name . "C") (status . 6)))))
    (let ((result (trx-filter-apply torrents "status:seed")))
      (should (= 1 (length result)))
      (should (equal "C" (cdr (assq 'name (elt result 0))))))))

(ert-deftest trx-filter-apply-label ()
  "Filter by label."
  (let ((torrents (vector '((name . "A") (labels . ["linux"]))
                          '((name . "B") (labels . ["video"]))
                          '((name . "C") (labels . ["linux" "iso"])))))
    (let ((result (trx-filter-apply torrents "label:linux")))
      (should (= 2 (length result))))))

(ert-deftest trx-filter-apply-negate ()
  "Negated filter."
  (let ((torrents (vector '((name . "Ubuntu") (status . 6))
                          '((name . "Fedora") (status . 4)))))
    (let ((result (trx-filter-apply torrents "!Ubuntu")))
      (should (= 1 (length result)))
      (should (equal "Fedora"
                     (cdr (assq 'name (elt result 0))))))))

(ert-deftest trx-filter-apply-negate-status ()
  "Negated status filter."
  (let ((torrents (vector '((name . "A") (status . 0))
                          '((name . "B") (status . 4))
                          '((name . "C") (status . 6)))))
    (let ((result (trx-filter-apply torrents "!status:stopped")))
      (should (= 2 (length result))))))

(ert-deftest trx-filter-apply-empty-result ()
  "Filter matching nothing."
  (let ((torrents (vector '((name . "foo")))))
    (should (= 0 (length (trx-filter-apply torrents "nonexistent"))))))

;;;;; trx-ffap-string

(ert-deftest trx-ffap-string-nil ()
  "Nil input returns nil."
  (should-not (trx-ffap-string nil)))

(ert-deftest trx-ffap-string-info-hash ()
  "Info hash is detected."
  (let ((hash "0123456789abcdef0123456789abcdef01234567"))
    (should (equal hash (trx-ffap-string hash)))))


;;;; HTTP/RPC parsing

;;;;; trx--move-to-content

(ert-deftest trx--move-to-content-crlf ()
  "Find content after CRLF blank line."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello")
    (should (trx--move-to-content))
    (should (looking-at "hello"))))

(ert-deftest trx--move-to-content-lf ()
  "Find content after LF blank line."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\nContent-Length: 5\n\nhello")
    (should (trx--move-to-content))
    (should (looking-at "hello"))))

(ert-deftest trx--move-to-content-no-blank ()
  "Return nil when no blank line separator."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\nno blank line")
    (should-not (trx--move-to-content))))

;;;;; trx--content-finished-p

(ert-deftest trx--content-finished-p-complete ()
  "Complete content returns non-nil."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello")
    (should (trx--content-finished-p))))

(ert-deftest trx--content-finished-p-incomplete ()
  "Incomplete content returns nil."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\nContent-Length: 10\r\n\r\nhello")
    (should-not (trx--content-finished-p))))

(ert-deftest trx--content-finished-p-no-header ()
  "No Content-Length header returns nil."
  (with-temp-buffer
    (insert "HTTP/1.1 200 OK\r\n\r\nhello")
    (should-not (trx--content-finished-p))))

;;;;; trx--status

(ert-deftest trx--status-200-success ()
  "200 with success result does not signal."
  (with-temp-buffer
    (insert (concat "HTTP/1.1 200 OK\r\n"
                    "Content-Length: 30\r\n\r\n"
                    "{\"result\":\"success\""
                    ",\"arguments\":{}}"))
    (should-not (trx--status))))

(ert-deftest trx--status-200-failure ()
  "200 with non-success result signals `trx-failure'."
  (with-temp-buffer
    (insert (concat "HTTP/1.1 200 OK\r\n"
                    "Content-Length: 50\r\n\r\n"
                    "{\"result\":\"no such torrent\""
                    ",\"arguments\":{}}"))
    (should-error (trx--status) :type 'trx-failure)))

(ert-deftest trx--status-401 ()
  "401 signals `trx-unauthorized'."
  (with-temp-buffer
    (insert "HTTP/1.1 401 Unauthorized\r\n\r\n")
    (should-error (trx--status) :type 'trx-unauthorized)))

(ert-deftest trx--status-409 ()
  "409 extracts session ID and signals `trx-conflict'."
  (with-temp-buffer
    (insert (concat "HTTP/1.1 409 Conflict\r\n"
                    "X-Transmission-Session-Id: abc123\r\n\r\n"))
    (should-error (trx--status) :type 'trx-conflict)
    (should (equal 'abc123 trx-session-id))))

(ert-deftest trx--status-301 ()
  "301 signals `trx-wrong-rpc-path'."
  (with-temp-buffer
    (insert "HTTP/1.1 301 Moved\r\n\r\n")
    (should-error (trx--status) :type 'trx-wrong-rpc-path)))

(ert-deftest trx--status-404 ()
  "404 signals `trx-wrong-rpc-path'."
  (with-temp-buffer
    (insert "HTTP/1.1 404 Not Found\r\n\r\n")
    (should-error (trx--status) :type 'trx-wrong-rpc-path)))

(ert-deftest trx--status-421 ()
  "421 signals `trx-misdirected'."
  (with-temp-buffer
    (insert "HTTP/1.1 421 Misdirected Request\r\n\r\n")
    (should-error (trx--status) :type 'trx-misdirected)))

;;;;; trx--auth-string

(ert-deftest trx--auth-string-nil ()
  "No auth configured returns nil."
  (let ((trx-rpc-auth nil))
    (should-not (trx--auth-string))))

(ert-deftest trx--auth-string-with-credentials ()
  "Auth with username and password returns Basic auth header."
  (let ((trx-rpc-auth '(:username "admin" :password "secret")))
    (let ((result (trx--auth-string)))
      (should (string-prefix-p "Basic " result))
      (should (equal "admin:secret"
                     (base64-decode-string (substring result 6)))))))


;;;; Constants and data structures

(ert-deftest trx-status-names-length ()
  "Status names vector has 7 entries."
  (should (= 7 (length trx-status-names))))

(ert-deftest trx-priority-alist-values ()
  "Priority alist has expected values."
  (should (= -1 (cdr (assq 'low trx-priority-alist))))
  (should (= 0 (cdr (assq 'normal trx-priority-alist))))
  (should (= 1 (cdr (assq 'high trx-priority-alist)))))

(ert-deftest trx-mode-alist-values ()
  "Mode alist has expected values."
  (should (= 0 (cdr (assq 'session trx-mode-alist))))
  (should (= 1 (cdr (assq 'torrent trx-mode-alist))))
  (should (= 2 (cdr (assq 'unlimited trx-mode-alist)))))

(ert-deftest trx-schedules-sun-through-sat ()
  "Individual days are powers of 2."
  (let ((days '(sun mon tues wed thurs fri sat)))
    (dotimes (i 7)
      (should (= (ash 1 i)
                 (cdr (assq (nth i days) trx-schedules)))))))

(ert-deftest trx-schedules-composites ()
  "Weekday, weekend, all are correct composites."
  (let ((weekday (cdr (assq 'weekday trx-schedules)))
        (weekend (cdr (assq 'weekend trx-schedules)))
        (all (cdr (assq 'all trx-schedules))))
    (should (= weekday (logior 2 4 8 16 32)))
    (should (= weekend (logior 1 64)))
    (should (= all (logior weekday weekend)))))

(ert-deftest trx-file-symbols-contents ()
  "File symbols list is complete."
  (should (= 5 (length trx-file-symbols)))
  (dolist (sym '(:files-wanted :files-unwanted :priority-high
                 :priority-low :priority-normal))
    (should (memq sym trx-file-symbols))))


;;;; Geoip

(ert-deftest trx-geoip-retrieve-no-function ()
  "No geoip function returns nil."
  (let ((trx-geoip-function nil))
    (should-not (trx-geoip-retrieve "1.2.3.4"))))

(ert-deftest trx-geoip-retrieve-with-function ()
  "Custom geoip function is called."
  (let ((trx-geoip-function (lambda (_ip) "Testland"))
        (trx-geoip-use-cache nil))
    (should (equal "Testland" (trx-geoip-retrieve "1.2.3.4")))))

(ert-deftest trx-geoip-retrieve-with-cache ()
  "Caching stores and retrieves values when fn matches."
  (let* ((call-count 0)
         (fun (lambda (_ip)
                (cl-incf call-count)
                "Cached"))
         (trx-geoip-function fun)
         (trx-geoip-use-cache t)
         (trx-geoip-table (make-hash-table :test 'equal)))
    (put 'trx-geoip-table :fn fun)
    (should (equal "Cached" (trx-geoip-retrieve "1.2.3.4")))
    (should (equal "Cached" (trx-geoip-retrieve "1.2.3.4")))
    (should (= 1 call-count))))


;;;; URI detection (TRAMP guard)

(ert-deftest trx--uri-like-p-magnet ()
  "Magnet link is URI-like."
  (should (trx--uri-like-p "magnet:?xt=urn:btih:abc")))

(ert-deftest trx--uri-like-p-http ()
  "HTTP URL is URI-like."
  (should (trx--uri-like-p "http://example.com/foo.torrent")))

(ert-deftest trx--uri-like-p-https ()
  "HTTPS URL is URI-like."
  (should (trx--uri-like-p "https://example.com/foo.torrent")))

(ert-deftest trx--uri-like-p-udp ()
  "UDP tracker URL is URI-like."
  (should (trx--uri-like-p "udp://tracker.example.com:6969")))

(ert-deftest trx--uri-like-p-file ()
  "Local file path is not URI-like."
  (should-not (trx--uri-like-p "/tmp/foo.torrent")))

(ert-deftest trx--uri-like-p-hash ()
  "Bare info hash is not URI-like."
  (should-not
   (trx--uri-like-p "0123456789abcdef0123456789abcdef01234567")))

(ert-deftest trx--uri-like-p-nil ()
  "Nil is not URI-like."
  (should-not (trx--uri-like-p nil)))


;;;; Default directory helper

(ert-deftest trx--set-default-directory-sets-dir ()
  "Sets `default-directory' from torrent data."
  (let ((trx-torrent-vector
         (vector `((downloadDir . ,temporary-file-directory))))
        (default-directory "/"))
    (trx--set-default-directory)
    (should (equal (file-name-as-directory temporary-file-directory)
                   default-directory))))

(ert-deftest trx--set-default-directory-nonexistent ()
  "Does not set `default-directory' for nonexistent directory."
  (let ((trx-torrent-vector
         (vector '((downloadDir . "/nonexistent/path/42/"))))
        (default-directory "/"))
    (trx--set-default-directory)
    (should (equal "/" default-directory))))


;;;; Mode line summary

(ert-deftest trx--update-mode-line-no-crash ()
  "Does not error when `trx-torrent-vector' is nil."
  (let ((trx-torrent-vector nil))
    (should-not (trx--update-mode-line))))


;;;; Large value edge cases

(ert-deftest trx-percent-large-values ()
  "Large values do not overflow."
  (should (= 50.0 (trx-percent 5000000000 10000000000))))


;;;; Jackett integration

(require 'trx-jackett)

(ert-deftest trx-jackett--format-size-nil ()
  "Nil bytes returns ?."
  (should (equal "?" (trx-jackett--format-size nil))))

(ert-deftest trx-jackett--format-size-zero ()
  "Zero bytes returns ?."
  (should (equal "?" (trx-jackett--format-size 0))))

(ert-deftest trx-jackett--format-size-normal ()
  "Normal size returns human-readable string."
  (let ((result (trx-jackett--format-size 1073741824)))
    (should (stringp result))
    (should (string-match-p "G" result))))

(ert-deftest trx-jackett--format-age-nil ()
  "Nil date returns ?."
  (should (equal "?" (trx-jackett--format-age nil))))

(ert-deftest trx-jackett--format-age-empty ()
  "Empty string returns ?."
  (should (equal "?" (trx-jackett--format-age ""))))

(ert-deftest trx-jackett--url-basic ()
  "URL is well-formed with required parameters."
  (let ((trx-jackett-host "localhost")
        (trx-jackett-port 9117)
        (trx-jackett-api-key "testkey123")
        (trx-jackett-use-tls nil)
        (trx-jackett-categories nil))
    (let ((url (trx-jackett--url "ubuntu")))
      (should (string-prefix-p "http://localhost:9117/" url))
      (should (string-match-p "apikey" url))
      (should (string-match-p "ubuntu" url)))))

(ert-deftest trx-jackett--url-tls ()
  "TLS uses https scheme."
  (let ((trx-jackett-host "localhost")
        (trx-jackett-port 9117)
        (trx-jackett-api-key "testkey123")
        (trx-jackett-use-tls t)
        (trx-jackett-categories nil))
    (should (string-prefix-p "https://"
                             (trx-jackett--url "test")))))

(ert-deftest trx-jackett--api-key-direct ()
  "Direct API key is returned."
  (let ((trx-jackett-api-key "mykey"))
    (should (equal "mykey" (trx-jackett--api-key)))))

(ert-deftest trx-jackett--api-key-missing ()
  "Missing API key signals error when no source is available."
  (cl-letf (((symbol-function 'trx-jackett--api-key-from-config)
             (lambda () nil)))
    (let ((trx-jackett-api-key nil)
          (trx-jackett-host "nonexistent.test")
          (trx-jackett-port 9117))
      (should-error (trx-jackett--api-key) :type 'user-error))))

(provide 'trx-test)

;;; trx-test.el ends here
