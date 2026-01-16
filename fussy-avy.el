;;; fussy-avy.el --- Fussy/typo-tolerant avy jumping -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: Your Name
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (avy "0.5.0"))
;; Keywords: navigation, fussy, matching
;; URL: https://github.com/your-username/fussy-avy

;;; Commentary:

;; This package provides a typo-tolerant version of avy-goto-char-timer.
;; Instead of aborting when you mistype, it uses fussy matching to find
;; the best matches for what you probably meant.
;;
;; For example, typing "footar" will still match "foobar" because the
;; edit distance is small and it's obviously what you intended.

;;; Code:

(require 'avy)
(require 'cl-lib)

(defgroup fussy-avy nil
  "Fussy/typo-tolerant avy jumping."
  :group 'convenience
  :prefix "fussy-avy-")

(defcustom fussy-avy-timeout avy-timeout-seconds
  "Timeout in seconds between keystrokes before matching."
  :type 'number
  :group 'fussy-avy)

(defcustom fussy-avy-max-distance 2
  "Maximum edit distance to consider a match.
Higher values are more forgiving of typos but may produce false positives.
Set to 3 or 4 for max forgiving mode."
  :type 'integer
  :group 'fussy-avy)

(defcustom fussy-avy-min-input-length 3
  "Minimum input length before fussy matching kicks in.
Below this length, exact prefix matching is used.
Set to 2 for max forgiving mode."
  :type 'integer
  :group 'fussy-avy)

;;;###autoload
(defun fussy-avy-max-forgiving ()
  "Enable max forgiving settings for fussy-avy."
  (interactive)
  (setq fussy-avy-max-distance 4
        fussy-avy-min-input-length 2)
  (message "Fussy-avy max forgiving: distance=%d, min-length=%d"
           fussy-avy-max-distance fussy-avy-min-input-length))

(defvar fussy-avy--overlays nil
  "List of overlays used for highlighting matches.")

;;; Damerau-Levenshtein Distance Implementation

(defun fussy-avy--damerau-levenshtein (s1 s2)
  "Calculate Damerau-Levenshtein distance between S1 and S2.
This allows substitutions, insertions, deletions, and transpositions."
  (let* ((len1 (length s1))
         (len2 (length s2))
         (matrix (make-vector (1+ len1) nil)))
    ;; Initialize matrix
    (dotimes (i (1+ len1))
      (aset matrix i (make-vector (1+ len2) 0)))
    ;; Fill first row and column
    (dotimes (i (1+ len1))
      (aset (aref matrix i) 0 i))
    (dotimes (j (1+ len2))
      (aset (aref matrix 0) j j))
    ;; Fill rest of matrix
    (dotimes (i len1)
      (dotimes (j len2)
        (let* ((cost (if (eq (aref s1 i) (aref s2 j)) 0 1))
               (above (aref (aref matrix i) (1+ j)))
               (left (aref (aref matrix (1+ i)) j))
               (diag (aref (aref matrix i) j))
               (min-val (min (1+ above)
                             (1+ left)
                             (+ diag cost))))
          ;; Check for transposition
          (when (and (> i 0) (> j 0)
                     (eq (aref s1 i) (aref s2 (1- j)))
                     (eq (aref s1 (1- i)) (aref s2 j)))
            (setq min-val (min min-val
                               (+ (aref (aref matrix (1- i)) (1- j)) cost))))
          (aset (aref matrix (1+ i)) (1+ j) min-val))))
    (aref (aref matrix len1) len2)))

;;; Input Normalization

(defun fussy-avy--normalize-input (input)
  "Normalize INPUT for matching.
Converts spaces to a pattern that matches any non-word character."
  ;; Replace spaces with a marker we'll handle specially
  (replace-regexp-in-string " " "\0" input))

(defun fussy-avy--char-match-p (input-char target-char)
  "Return t if INPUT-CHAR matches TARGET-CHAR.
Space in input matches any non-word character in target."
  (cond
   ;; Null byte (was space) matches any non-word char
   ((eq input-char ?\0)
    (not (or (and (>= target-char ?a) (<= target-char ?z))
             (and (>= target-char ?A) (<= target-char ?Z))
             (and (>= target-char ?0) (<= target-char ?9))
             (eq target-char ?_))))
   ;; Case-insensitive letter match
   (t (eq (downcase input-char) (downcase target-char)))))

(defun fussy-avy--fussy-distance (input target)
  "Calculate fussy distance between INPUT and TARGET.
Spaces in INPUT match any non-word character.
Returns the edit distance, or nil if target is too short."
  (if (< (length target) (length input))
      nil
    (let* ((input-norm (fussy-avy--normalize-input (downcase input)))
           (target-prefix (downcase (substring target 0 (length input))))
           (len (length input-norm))
           (exact-match t))
      ;; First check for exact/near-exact match with space handling
      (dotimes (i len)
        (unless (fussy-avy--char-match-p (aref input-norm i) (aref target-prefix i))
          (setq exact-match nil)))
      (if exact-match
          0
        ;; Fall back to edit distance, treating spaces specially
        ;; For edit distance, replace the null bytes back and compare
        (let ((input-for-dist (replace-regexp-in-string "\0" "-" input-norm)))
          (fussy-avy--damerau-levenshtein input-for-dist target-prefix))))))

;;; Buffer Scanning

(defun fussy-avy--collect-candidates ()
  "Collect all potential jump targets in the visible buffer.
Returns list of (text . position) for symbols/words."
  (let ((candidates '())
        (win-start (window-start))
        (win-end (window-end nil t))
        ;; Match symbols (including hyphens) or words
        (pattern "\\_<\\(\\sw\\|\\s_\\)+"))
    (save-excursion
      (goto-char win-start)
      (while (re-search-forward pattern win-end t)
        (let ((text (match-string-no-properties 0))
              (pos (match-beginning 0)))
          (push (cons text pos) candidates))))
    (nreverse candidates)))

(defun fussy-avy--get-buffer-text-at (pos len)
  "Get LEN characters of buffer text starting at POS."
  (buffer-substring-no-properties pos (min (+ pos len) (point-max))))

(defun fussy-avy--find-matches (input)
  "Find all candidates matching INPUT in the visible buffer.
Returns list of (position . score) pairs, sorted by score."
  (when (> (length input) 0)
    (let ((candidates (fussy-avy--collect-candidates))
          (input-len (length input))
          (matches '()))
      (dolist (cand candidates)
        (let* ((pos (cdr cand))
               ;; Compare against buffer text at position, not just the word
               (buffer-text (fussy-avy--get-buffer-text-at pos input-len))
               (distance (fussy-avy--fussy-distance input buffer-text)))
          (when (and distance
                     (or (< input-len fussy-avy-min-input-length)
                         (<= distance fussy-avy-max-distance)))
            ;; For short input, only accept exact prefix (distance 0)
            (when (or (>= input-len fussy-avy-min-input-length)
                      (= distance 0))
              (push (cons pos distance) matches)))))
      ;; Sort by score (lower is better), then by position
      (sort matches (lambda (a b)
                      (if (= (cdr a) (cdr b))
                          (< (car a) (car b))
                        (< (cdr a) (cdr b))))))))

;;; Overlay Management (Highlighting)

(defun fussy-avy--make-overlay (pos input-len)
  "Create a highlight overlay at POS for INPUT-LEN characters."
  (let ((ov (make-overlay pos (+ pos input-len))))
    (overlay-put ov 'face 'avy-goto-char-timer-face)
    (overlay-put ov 'priority 100)
    (push ov fussy-avy--overlays)
    ov))

(defun fussy-avy--clear-overlays ()
  "Remove all fussy-avy overlays."
  (mapc #'delete-overlay fussy-avy--overlays)
  (setq fussy-avy--overlays nil))

(defun fussy-avy--update-overlays (matches input-len)
  "Update overlays to highlight MATCHES with INPUT-LEN."
  (fussy-avy--clear-overlays)
  (dolist (match matches)
    (let ((pos (car match)))
      (fussy-avy--make-overlay pos input-len))))

;;; Input Loop

(defun fussy-avy--read-input-with-highlights ()
  "Read input showing live highlights. Returns (input . matches)."
  (let ((input "")
        (matches nil)
        (continue t))
    (unwind-protect
        (progn
          (while continue
            (setq matches (fussy-avy--find-matches input))
            (fussy-avy--update-overlays matches (length input))
            (let* ((match-count (length matches))
                   (prompt (format "Fussy avy [%d]: %s"
                                   match-count input))
                   (char (read-char prompt nil fussy-avy-timeout)))
              (cond
               ;; Timeout - we're done
               ((null char)
                (setq continue nil))
               ;; Backspace
               ((memq char '(?\C-h ?\C-? 127))
                (when (> (length input) 0)
                  (setq input (substring input 0 -1))))
               ;; Enter/Return - we're done
               ((memq char '(?\C-m ?\C-j))
                (setq continue nil))
               ;; Escape/C-g - abort
               ((memq char '(?\C-g 27 ?\e))
                (fussy-avy--clear-overlays)
                (keyboard-quit))
               ;; Regular character (printable)
               ((and (characterp char) (>= char 32) (<= char 126))
                (setq input (concat input (char-to-string char)))))))
          (cons input matches))
      ;; Cleanup overlays on any exit
      (fussy-avy--clear-overlays))))

;;; Main Entry Point

;;;###autoload
(defun fussy-avy-goto-char-timer ()
  "Jump to a symbol using fussy/typo-tolerant matching.
Type characters to narrow down matches. Matches are highlighted live.
After a timeout or pressing RET, avy hints are shown for all matches.

Unlike `avy-goto-char-timer', this tolerates typos. For example,
typing `footar' will still match `foobar'.

Spaces in input match any non-word character, so `with ' matches `with-'."
  (interactive)
  (let* ((result (fussy-avy--read-input-with-highlights))
         (input (car result))
         (matches (cdr result)))
    (cond
     ((string-empty-p input)
      (message "No input"))
     ((null matches)
      (message "No matches for '%s'" input))
     ((= (length matches) 1)
      ;; Single match - jump directly
      (goto-char (caar matches)))
     (t
      ;; Multiple matches - use avy
      (avy-process
       (mapcar (lambda (m) (cons (car m) (selected-window)))
               matches))))))

;;; Evil Integration (optional)

(with-eval-after-load 'evil
  (evil-define-motion evil-fussy-avy-goto-char-timer (&optional count)
    "Evil motion for `fussy-avy-goto-char-timer'.
COUNT is currently unused but kept for compatibility."
    :type inclusive
    :jump t
    :repeat abort
    (ignore count)
    (fussy-avy-goto-char-timer)))

(provide 'fussy-avy)
;;; fussy-avy.el ends here
