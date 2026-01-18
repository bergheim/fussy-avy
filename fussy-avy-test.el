;;; fussy-avy-test.el --- Tests for fussy-avy -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for fussy-avy fussy matching functionality.

;;; Code:

(require 'ert)
(require 'fussy-avy)

;;; Damerau-Levenshtein Tests

(ert-deftest fussy-avy-test-levenshtein-identical ()
  "Identical strings have distance 0."
  (should (= 0 (fussy-avy--damerau-levenshtein "hello" "hello")))
  (should (= 0 (fussy-avy--damerau-levenshtein "" "")))
  (should (= 0 (fussy-avy--damerau-levenshtein "a" "a"))))

(ert-deftest fussy-avy-test-levenshtein-substitution ()
  "Single character substitution is distance 1."
  (should (= 1 (fussy-avy--damerau-levenshtein "cat" "bat")))
  (should (= 1 (fussy-avy--damerau-levenshtein "footar" "foobar")))
  (should (= 3 (fussy-avy--damerau-levenshtein "abc" "xyz"))))

(ert-deftest fussy-avy-test-levenshtein-insertion-deletion ()
  "Insertions and deletions."
  (should (= 1 (fussy-avy--damerau-levenshtein "cat" "cats")))
  (should (= 1 (fussy-avy--damerau-levenshtein "cats" "cat")))
  (should (= 3 (fussy-avy--damerau-levenshtein "" "abc")))
  (should (= 3 (fussy-avy--damerau-levenshtein "abc" ""))))

(ert-deftest fussy-avy-test-levenshtein-transposition ()
  "Transpositions count as 1 edit (Damerau extension)."
  (should (= 1 (fussy-avy--damerau-levenshtein "teh" "the")))
  (should (= 1 (fussy-avy--damerau-levenshtein "ab" "ba")))
  (should (= 1 (fussy-avy--damerau-levenshtein "abcd" "abdc"))))

;;; Character Matching Tests

(ert-deftest fussy-avy-test-char-match-same ()
  "Same characters match."
  (should (fussy-avy--char-match-p ?a ?a))
  (should (fussy-avy--char-match-p ?z ?z))
  (should (fussy-avy--char-match-p ?5 ?5)))

(ert-deftest fussy-avy-test-char-match-case-insensitive ()
  "Matching is case-insensitive."
  (should (fussy-avy--char-match-p ?a ?A))
  (should (fussy-avy--char-match-p ?Z ?z))
  (should (fussy-avy--char-match-p ?M ?m)))

(ert-deftest fussy-avy-test-char-match-space-nonword ()
  "Space (as null byte) matches non-word characters."
  (should (fussy-avy--char-match-p ?\0 ?-))
  (should (fussy-avy--char-match-p ?\0 ? ))
  (should (fussy-avy--char-match-p ?\0 ?.))
  (should (fussy-avy--char-match-p ?\0 ?:))
  (should (fussy-avy--char-match-p ?\0 ?\n)))

(ert-deftest fussy-avy-test-char-match-space-not-word ()
  "Space does not match word characters."
  (should-not (fussy-avy--char-match-p ?\0 ?a))
  (should-not (fussy-avy--char-match-p ?\0 ?Z))
  (should-not (fussy-avy--char-match-p ?\0 ?5))
  (should-not (fussy-avy--char-match-p ?\0 ?_)))

;;; Fussy Distance Tests

(ert-deftest fussy-avy-test-fussy-distance-exact ()
  "Exact prefix match is distance 0."
  (should (= 0 (fussy-avy--fussy-distance "foo" "foobar")))
  (should (= 0 (fussy-avy--fussy-distance "hello" "hello")))
  (should (= 0 (fussy-avy--fussy-distance "a" "abc"))))

(ert-deftest fussy-avy-test-fussy-distance-case-insensitive ()
  "Matching is case-insensitive."
  (should (= 0 (fussy-avy--fussy-distance "foo" "FOObar")))
  (should (= 0 (fussy-avy--fussy-distance "EVIL" "evil"))))

(ert-deftest fussy-avy-test-fussy-distance-too-short ()
  "Returns nil if target is shorter than input."
  (should (null (fussy-avy--fussy-distance "foobar" "foo")))
  (should (null (fussy-avy--fussy-distance "abc" "ab"))))

(ert-deftest fussy-avy-test-fussy-distance-typos ()
  "Typos result in appropriate distance."
  (should (= 1 (fussy-avy--fussy-distance "footar" "foobar")))
  (should (= 1 (fussy-avy--fussy-distance "teh" "the")))
  (should (= 2 (fussy-avy--fussy-distance "defcstm" "defcustom"))))

(ert-deftest fussy-avy-test-fussy-distance-space-matches-nonword ()
  "Space in input matches non-word chars in target."
  (should (= 0 (fussy-avy--fussy-distance "with " "with-eval")))
  (should (= 0 (fussy-avy--fussy-distance "eval " "eval-after")))
  (should (= 0 (fussy-avy--fussy-distance "foo bar" "foo-bar-baz"))))

(ert-deftest fussy-avy-test-fussy-distance-space-matches-space ()
  "Space in input matches space in target."
  (should (= 0 (fussy-avy--fussy-distance "evil i" "Evil Integration"))))

;;; Buffer Matching Tests
;; Match format is now (position window score)

(defun fussy-avy-test--match-pos (match)
  "Get position from MATCH."
  (nth 0 match))

(defun fussy-avy-test--match-score (match)
  "Get score from MATCH."
  (nth 2 match))

(defmacro fussy-avy-test--with-buffer (content &rest body)
  "Execute BODY in a temp buffer with CONTENT, mocking window functions."
  (declare (indent 1))
  `(let ((test-buf (generate-new-buffer " *fussy-avy-test*")))
     (unwind-protect
         (progn
           (set-buffer test-buf)
           (insert ,content)
           (goto-char (point-min))
           (let ((fussy-avy-all-windows nil)
                 (test-win (selected-window)))
             (cl-letf (((symbol-function 'window-start)
                        (lambda (&optional _) (point-min)))
                       ((symbol-function 'window-end)
                        (lambda (&optional _ _) (point-max)))
                       ((symbol-function 'window-buffer)
                        (lambda (&optional _) test-buf))
                       ((symbol-function 'fussy-avy--collect-candidates-in-window)
                        (lambda (_win)
                          ;; Collect from test buffer directly
                          (let ((candidates '())
                                (pattern "\\_<\\(\\sw\\|\\s_\\)+"))
                            (with-current-buffer test-buf
                              (save-excursion
                                (goto-char (point-min))
                                (while (re-search-forward pattern (point-max) t)
                                  (push (list (match-string-no-properties 0)
                                              (match-beginning 0)
                                              test-win)
                                        candidates))))
                            (nreverse candidates))))
                       ((symbol-function 'fussy-avy--get-windows)
                        (lambda () (list test-win)))
                       ((symbol-function 'fussy-avy--get-buffer-text-at)
                        (lambda (pos len &optional _win)
                          (with-current-buffer test-buf
                            (buffer-substring-no-properties
                             pos (min (+ pos len) (point-max)))))))
               ,@body)))
       (kill-buffer test-buf))))

(ert-deftest fussy-avy-test-find-matches-basic ()
  "Basic matching in buffer."
  (fussy-avy-test--with-buffer "aaa bbb foobar"
    (let ((matches (fussy-avy--find-matches "foo")))
      (should (= 1 (length matches)))
      (should (= 9 (fussy-avy-test--match-pos (car matches)))))))

(ert-deftest fussy-avy-test-find-matches-fussy ()
  "Fussy matching finds typo'd input."
  (fussy-avy-test--with-buffer "hello world foobar"
    (let ((matches (fussy-avy--find-matches "footar")))
      (should (= 1 (length matches)))
      (should (= 13 (fussy-avy-test--match-pos (car matches))))
      (should (= 1 (fussy-avy-test--match-score (car matches)))))))

(ert-deftest fussy-avy-test-find-matches-space-hyphen ()
  "Space matches hyphen in symbol names."
  (fussy-avy-test--with-buffer "with-eval-after-load some-other-thing"
    (emacs-lisp-mode)
    (let ((matches (fussy-avy--find-matches "with ")))
      (should (>= (length matches) 1))
      (should (= 1 (fussy-avy-test--match-pos (car matches)))))))

(ert-deftest fussy-avy-test-find-matches-across-words ()
  "Input with space matches across word boundaries."
  (fussy-avy-test--with-buffer ";;; Evil Integration (optional)"
    (let ((matches (fussy-avy--find-matches "evil i")))
      (should (= 1 (length matches)))
      (should (= 0 (fussy-avy-test--match-score (car matches)))))))

(ert-deftest fussy-avy-test-find-matches-no-match ()
  "Returns nil when nothing matches."
  (fussy-avy-test--with-buffer "hello world"
    (let ((matches (fussy-avy--find-matches "zzzzz")))
      (should (null matches)))))

(ert-deftest fussy-avy-test-find-matches-sorted-by-score ()
  "Matches are sorted by score (exact first)."
  (fussy-avy-test--with-buffer "foobar foobaz"
    (let ((matches (fussy-avy--find-matches "foobar")))
      (should (>= (length matches) 1))
      ;; First match should be exact (score 0)
      (should (= 0 (fussy-avy-test--match-score (car matches)))))))

;;; Multi-Window Tests

(ert-deftest fussy-avy-test-all-windows-nil ()
  "When fussy-avy-all-windows is nil, only selected window."
  (let ((fussy-avy-all-windows nil))
    (should (= 1 (length (fussy-avy--get-windows))))
    (should (eq (selected-window) (car (fussy-avy--get-windows))))))

(ert-deftest fussy-avy-test-all-windows-t ()
  "When fussy-avy-all-windows is t, get all frame windows."
  (let ((fussy-avy-all-windows t))
    ;; Should return at least the selected window
    (should (>= (length (fussy-avy--get-windows)) 1))
    (should (memq (selected-window) (fussy-avy--get-windows)))))

;;; Face Tests

(ert-deftest fussy-avy-test-faces-defined ()
  "Custom faces are defined."
  (should (facep 'fussy-avy-match-exact))
  (should (facep 'fussy-avy-match-fuzzy)))

(ert-deftest fussy-avy-test-match-scores ()
  "Exact matches have score 0, fuzzy matches have score > 0."
  (fussy-avy-test--with-buffer "foobar foobaz"
    (let ((matches (fussy-avy--find-matches "foobar")))
      ;; Exact match for "foobar" should have score 0
      (should (= 0 (fussy-avy-test--match-score (car matches)))))
    (let ((matches (fussy-avy--find-matches "footar")))
      ;; Fuzzy match should have score > 0
      (should (> (fussy-avy-test--match-score (car matches)) 0)))))

;;; Prompt Formatting Tests

(ert-deftest fussy-avy-test-prompt-no-matches ()
  "Prompt with no matches shows count only."
  (let ((prompt (fussy-avy--format-prompt "xyz" nil)))
    (should (string-match-p "\\[0\\]" prompt))
    (should (string-match-p "xyz" prompt))))

(ert-deftest fussy-avy-test-prompt-with-matches ()
  "Prompt with matches shows count and preview."
  (fussy-avy-test--with-buffer "foobar baz"
    (let* ((matches (fussy-avy--find-matches "foo"))
           (prompt (fussy-avy--format-prompt "foo" matches)))
      ;; Should show count
      (should (string-match-p "\\[1" prompt))
      ;; Should show preview of matched text
      (should (string-match-p "foobar" prompt))
      ;; Should show input
      (should (string-match-p "foo" prompt)))))

(ert-deftest fussy-avy-test-prompt-fuzzy-shows-score ()
  "Prompt shows score indicator for fuzzy matches."
  (fussy-avy-test--with-buffer "foobar baz"
    (let* ((matches (fussy-avy--find-matches "footar"))
           (prompt (fussy-avy--format-prompt "footar" matches)))
      ;; Should show score indicator ~1
      (should (string-match-p "~1" prompt)))))

;;; Max Forgiving Mode Tests

(ert-deftest fussy-avy-test-max-forgiving-mode ()
  "Max forgiving mode sets correct values."
  (let ((fussy-avy-max-distance 2)
        (fussy-avy-min-input-length 3))
    (fussy-avy-max-forgiving)
    (should (= 4 fussy-avy-max-distance))
    (should (= 2 fussy-avy-min-input-length))))

;;; Consult Integration Tests

(ert-deftest fussy-avy-test-consult-lookup ()
  "Consult lookup function works with correct signature."
  (let ((test-cands '(("buf:1: hello" . (1 nil 0))
                      ("buf:2: world" . (10 nil 0)))))
    ;; Basic lookup
    (should (equal (fussy-avy--consult-lookup "buf:1: hello" test-cands)
                   '(1 nil 0)))
    ;; With extra args (like consult passes)
    (should (equal (fussy-avy--consult-lookup "buf:2: world" test-cands 'extra 'args)
                   '(10 nil 0)))
    ;; Not found
    (should (null (fussy-avy--consult-lookup "nonexistent" test-cands)))))

(ert-deftest fussy-avy-test-consult-function-exists ()
  "Consult integration function is defined."
  (should (fboundp 'consult-fussy-avy))
  (should (fboundp 'fussy-avy--consult-candidates)))

;;; Evil Integration Tests

(ert-deftest fussy-avy-test-evil-function-not-defined-without-evil ()
  "Evil motion is not defined until evil is loaded.
This tests that evil-fussy-avy-goto-char-timer is defined via
with-eval-after-load, not unconditionally."
  ;; If evil is not loaded, the function should not exist
  ;; If evil IS loaded (e.g., in test environment), the function should exist
  ;; Either way, the symbol should not cause an error when quoted
  (should (symbolp 'evil-fussy-avy-goto-char-timer))
  ;; The function should only be bound if evil is loaded
  (if (featurep 'evil)
      (should (fboundp 'evil-fussy-avy-goto-char-timer))
    (should-not (fboundp 'evil-fussy-avy-goto-char-timer))))

(ert-deftest fussy-avy-test-evil-function-defined-after-evil ()
  "Evil motion is defined after evil is loaded."
  (skip-unless (locate-library "evil"))
  (require 'evil)
  (should (fboundp 'evil-fussy-avy-goto-char-timer))
  ;; Verify it's actually a function that calls fussy-avy-goto-char-timer
  (should (commandp 'evil-fussy-avy-goto-char-timer)))

(provide 'fussy-avy-test)
;;; fussy-avy-test.el ends here
