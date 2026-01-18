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
             (cl-letf (((symbol-function 'fussy-avy--get-windows)
                        (lambda () (list test-win)))
                       ((symbol-function 'fussy-avy--get-buffer-text-at)
                        (lambda (pos len &optional _win)
                          (with-current-buffer test-buf
                            (buffer-substring-no-properties
                             pos (min (+ pos len) (point-max))))))
                       ((symbol-function 'fussy-avy--find-matches-in-window)
                        (lambda (_win input)
                          ;; Scan test buffer directly
                          (let ((input-len (length input))
                                (matches '()))
                            (with-current-buffer test-buf
                              (save-excursion
                                (goto-char (point-min))
                                (while (< (point) (point-max))
                                  (let* ((text (buffer-substring-no-properties
                                                (point)
                                                (min (+ (point) input-len) (point-max))))
                                         (distance (fussy-avy--fussy-distance input text)))
                                    (when (and distance
                                               (or (< input-len fussy-avy-min-input-length)
                                                   (<= distance fussy-avy-max-distance))
                                               (or (>= input-len fussy-avy-min-input-length)
                                                   (= distance 0)))
                                      (push (list (point) test-win distance) matches)))
                                  (forward-char 1))))
                            matches)))
                       ((symbol-function 'fussy-avy-orderless--find-matches-in-window)
                        (lambda (_win input)
                          ;; Orderless matching in test buffer
                          (let* ((tokens (fussy-avy-orderless--split-tokens input))
                                 (first-token (car tokens))
                                 (matches '()))
                            (with-current-buffer test-buf
                              (let* ((text (buffer-substring-no-properties (point-min) (point-max)))
                                     (sentences (fussy-avy-orderless--get-sentences-from-text text 1)))
                                (dolist (sentence-bounds sentences)
                                  (let* ((sent-start (car sentence-bounds))
                                         (sent-end (cdr sentence-bounds))
                                         (sentence-text (buffer-substring-no-properties sent-start sent-end))
                                         (all-match t)
                                         (first-token-match nil))
                                    ;; Check if all tokens match in this sentence
                                    (dolist (token tokens)
                                      (let ((match (fussy-avy-orderless--find-word-in-text token sentence-text)))
                                        (unless match
                                          (setq all-match nil))))
                                    ;; Find first token's position
                                    (when all-match
                                      (setq first-token-match
                                            (fussy-avy-orderless--find-word-in-text first-token sentence-text)))
                                    ;; If all tokens matched, add the first token's position
                                    (when (and all-match first-token-match)
                                      (let ((pos (+ sent-start (car first-token-match)))
                                            (score (cdr first-token-match)))
                                        (push (list pos test-win score) matches)))))))
                            matches))))
               ,@body)))
       (kill-buffer test-buf))))

(ert-deftest fussy-avy-test-find-matches-basic ()
  "Basic matching in buffer."
  (fussy-avy-test--with-buffer "aaa bbb foobar"
    (let ((matches (fussy-avy--find-matches "foo")))
      (should (>= (length matches) 1))
      ;; Best match (first after sorting) should be exact at position 9
      (should (= 9 (fussy-avy-test--match-pos (car matches))))
      (should (= 0 (fussy-avy-test--match-score (car matches)))))))

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

(ert-deftest fussy-avy-test-find-matches-mid-symbol ()
  "Matches text in the middle of symbols, not just at start."
  (fussy-avy-test--with-buffer "fussy-avy-min-input-length"
    (let ((matches (fussy-avy--find-matches "avy")))
      (should (>= (length matches) 1))
      ;; Should find 'avy' at position 7 (inside fussy-avy-...)
      (should (= 7 (fussy-avy-test--match-pos (car matches)))))))

(ert-deftest fussy-avy-test-find-matches-mid-symbol-with-space ()
  "Matches 'avy min' inside 'fussy-avy-min-input-length'."
  (fussy-avy-test--with-buffer "fussy-avy-min-input-length"
    (let ((matches (fussy-avy--find-matches "avy min")))
      (should (>= (length matches) 1))
      ;; Should find 'avy-min' at position 7
      (should (= 7 (fussy-avy-test--match-pos (car matches)))))))

(ert-deftest fussy-avy-test-find-matches-anywhere ()
  "Matches can be found anywhere in text, not just symbol boundaries."
  (fussy-avy-test--with-buffer "the quick brown fox"
    (let ((matches (fussy-avy--find-matches "ick")))
      (should (>= (length matches) 1))
      ;; Should find 'ick' at position 7 (inside 'quick')
      (should (= 7 (fussy-avy-test--match-pos (car matches)))))))

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
      ;; Should show count (at least 1)
      (should (string-match-p "\\[[0-9]" prompt))
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

;;; Orderless Sentence Matching Tests
;; New feature: space-separated tokens match within sentences (like orderless)
;; All tokens must exist in the sentence, jump target = first token's position

(ert-deftest fussy-avy-test-orderless-basic ()
  "Basic case: 'fox meme' jumps to 'fox' when both words in sentence."
  :tags '(orderless)
  (fussy-avy-test--with-buffer "a fox trout on a meme"
    (let ((matches (fussy-avy-orderless--find-matches "fox meme")))
      (should (= 1 (length matches)))
      ;; Should jump to "fox" at position 3
      (should (= 3 (fussy-avy-test--match-pos (car matches)))))))

(ert-deftest fussy-avy-test-orderless-single-word ()
  "Single word degrades to regular matching."
  :tags '(orderless)
  (fussy-avy-test--with-buffer "a fox trout on a meme"
    (let ((matches (fussy-avy-orderless--find-matches "fox")))
      (should (>= (length matches) 1))
      ;; Should find "fox" at position 3
      (should (= 3 (fussy-avy-test--match-pos (car matches)))))))

(ert-deftest fussy-avy-test-orderless-token-order ()
  "Token order in input doesn't matter for matching, first token = jump target."
  :tags '(orderless)
  (fussy-avy-test--with-buffer "a fox trout on a meme"
    (let ((matches (fussy-avy-orderless--find-matches "meme fox")))
      (should (= 1 (length matches)))
      ;; Should jump to "meme" (first token) at position 18
      (should (= 18 (fussy-avy-test--match-pos (car matches)))))))

(ert-deftest fussy-avy-test-orderless-fuzzy-first-token ()
  "Fuzzy matching works on first token."
  :tags '(orderless)
  (fussy-avy-test--with-buffer "a fox trout on a meme"
    (let ((matches (fussy-avy-orderless--find-matches "foz meme")))
      (should (= 1 (length matches)))
      ;; Should jump to "fox" (fuzzy match for "foz") at position 3
      (should (= 3 (fussy-avy-test--match-pos (car matches)))))))

(ert-deftest fussy-avy-test-orderless-fuzzy-second-token ()
  "Fuzzy matching works on second token."
  :tags '(orderless)
  (fussy-avy-test--with-buffer "a fox trout on a meme"
    (let ((matches (fussy-avy-orderless--find-matches "fox mem")))
      (should (= 1 (length matches)))
      ;; Should jump to "fox" at position 3
      (should (= 3 (fussy-avy-test--match-pos (car matches)))))))

(ert-deftest fussy-avy-test-orderless-fuzzy-both-tokens ()
  "Fuzzy matching works on both tokens."
  :tags '(orderless)
  (fussy-avy-test--with-buffer "a fox trout on a meme"
    (let ((matches (fussy-avy-orderless--find-matches "foz mem")))
      (should (= 1 (length matches)))
      ;; Should jump to "fox" at position 3
      (should (= 3 (fussy-avy-test--match-pos (car matches)))))))

(ert-deftest fussy-avy-test-orderless-no-match-missing-token ()
  "No match when a token is not in the sentence."
  :tags '(orderless)
  (fussy-avy-test--with-buffer "a fox trout on a meme"
    (let ((matches (fussy-avy-orderless--find-matches "fox elephant")))
      (should (null matches)))))

(ert-deftest fussy-avy-test-orderless-multiple-sentences-one-matches ()
  "Only the sentence containing all tokens matches."
  :tags '(orderless)
  (fussy-avy-test--with-buffer "the dog runs. a fox trout on a meme."
    (let ((matches (fussy-avy-orderless--find-matches "fox meme")))
      (should (= 1 (length matches)))
      ;; Should jump to "fox" in second sentence at position 17
      (should (= 17 (fussy-avy-test--match-pos (car matches)))))))

(ert-deftest fussy-avy-test-orderless-multiple-sentences-same-word ()
  "When first token appears in multiple sentences, only match sentence with all tokens."
  :tags '(orderless)
  (fussy-avy-test--with-buffer "a fox runs. a fox trout on a meme."
    (let ((matches (fussy-avy-orderless--find-matches "fox meme")))
      (should (= 1 (length matches)))
      ;; Should jump to "fox" in SECOND sentence (position 15), not first
      (should (= 15 (fussy-avy-test--match-pos (car matches)))))))

(ert-deftest fussy-avy-test-orderless-three-tokens ()
  "Three tokens all present in sentence."
  :tags '(orderless)
  (fussy-avy-test--with-buffer "a fox trout on a meme"
    (let ((matches (fussy-avy-orderless--find-matches "fox trout meme")))
      (should (= 1 (length matches)))
      ;; Should jump to "fox" at position 3
      (should (= 3 (fussy-avy-test--match-pos (car matches)))))))

(ert-deftest fussy-avy-test-orderless-jump-to-first-token ()
  "Always jump to first token position, even when matched out of order."
  :tags '(orderless)
  (fussy-avy-test--with-buffer "a fox trout on a meme"
    (let ((matches (fussy-avy-orderless--find-matches "trout fox")))
      (should (= 1 (length matches)))
      ;; Should jump to "trout" (first token) at position 7
      (should (= 7 (fussy-avy-test--match-pos (car matches)))))))

(provide 'fussy-avy-test)
;;; fussy-avy-test.el ends here
