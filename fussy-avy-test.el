;;; fussy-avy-test.el --- Tests for fussy-avy -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for fussy-avy fuzzy matching functionality.

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

;;; Fuzzy Distance Tests

(ert-deftest fussy-avy-test-fuzzy-distance-exact ()
  "Exact prefix match is distance 0."
  (should (= 0 (fussy-avy--fuzzy-distance "foo" "foobar")))
  (should (= 0 (fussy-avy--fuzzy-distance "hello" "hello")))
  (should (= 0 (fussy-avy--fuzzy-distance "a" "abc"))))

(ert-deftest fussy-avy-test-fuzzy-distance-case-insensitive ()
  "Matching is case-insensitive."
  (should (= 0 (fussy-avy--fuzzy-distance "foo" "FOObar")))
  (should (= 0 (fussy-avy--fuzzy-distance "EVIL" "evil"))))

(ert-deftest fussy-avy-test-fuzzy-distance-too-short ()
  "Returns nil if target is shorter than input."
  (should (null (fussy-avy--fuzzy-distance "foobar" "foo")))
  (should (null (fussy-avy--fuzzy-distance "abc" "ab"))))

(ert-deftest fussy-avy-test-fuzzy-distance-typos ()
  "Typos result in appropriate distance."
  (should (= 1 (fussy-avy--fuzzy-distance "footar" "foobar")))
  (should (= 1 (fussy-avy--fuzzy-distance "teh" "the")))
  (should (= 2 (fussy-avy--fuzzy-distance "defcstm" "defcustom"))))

(ert-deftest fussy-avy-test-fuzzy-distance-space-matches-nonword ()
  "Space in input matches non-word chars in target."
  (should (= 0 (fussy-avy--fuzzy-distance "with " "with-eval")))
  (should (= 0 (fussy-avy--fuzzy-distance "eval " "eval-after")))
  (should (= 0 (fussy-avy--fuzzy-distance "foo bar" "foo-bar-baz"))))

(ert-deftest fussy-avy-test-fuzzy-distance-space-matches-space ()
  "Space in input matches space in target."
  (should (= 0 (fussy-avy--fuzzy-distance "evil i" "Evil Integration"))))

;;; Buffer Matching Tests

(ert-deftest fussy-avy-test-find-matches-basic ()
  "Basic matching in buffer."
  (with-temp-buffer
    (insert "aaa bbb foobar")
    (cl-letf (((symbol-function 'window-start) (lambda (&optional _) (point-min)))
              ((symbol-function 'window-end) (lambda (&optional _ _) (point-max))))
      (let ((matches (fussy-avy--find-matches "foo")))
        (should (= 1 (length matches)))
        (should (= 9 (caar matches)))))))

(ert-deftest fussy-avy-test-find-matches-fuzzy ()
  "Fuzzy matching finds typo'd input."
  (with-temp-buffer
    (insert "hello world foobar")
    (cl-letf (((symbol-function 'window-start) (lambda (&optional _) (point-min)))
              ((symbol-function 'window-end) (lambda (&optional _ _) (point-max))))
      (let ((matches (fussy-avy--find-matches "footar")))
        (should (= 1 (length matches)))
        (should (= 13 (caar matches)))
        (should (= 1 (cdar matches)))))))

(ert-deftest fussy-avy-test-find-matches-space-hyphen ()
  "Space matches hyphen in symbol names."
  (with-temp-buffer
    (emacs-lisp-mode)
    (insert "with-eval-after-load some-other-thing")
    (cl-letf (((symbol-function 'window-start) (lambda (&optional _) (point-min)))
              ((symbol-function 'window-end) (lambda (&optional _ _) (point-max))))
      (let ((matches (fussy-avy--find-matches "with ")))
        (should (>= (length matches) 1))
        (should (= 1 (caar matches)))))))

(ert-deftest fussy-avy-test-find-matches-across-words ()
  "Input with space matches across word boundaries."
  (with-temp-buffer
    (insert ";;; Evil Integration (optional)")
    (cl-letf (((symbol-function 'window-start) (lambda (&optional _) (point-min)))
              ((symbol-function 'window-end) (lambda (&optional _ _) (point-max))))
      (let ((matches (fussy-avy--find-matches "evil i")))
        (should (= 1 (length matches)))
        (should (= 0 (cdar matches)))))))

(ert-deftest fussy-avy-test-find-matches-no-match ()
  "Returns nil when nothing matches."
  (with-temp-buffer
    (insert "hello world")
    (cl-letf (((symbol-function 'window-start) (lambda (&optional _) (point-min)))
              ((symbol-function 'window-end) (lambda (&optional _ _) (point-max))))
      (let ((matches (fussy-avy--find-matches "zzzzz")))
        (should (null matches))))))

(ert-deftest fussy-avy-test-find-matches-sorted-by-score ()
  "Matches are sorted by score (exact first)."
  (with-temp-buffer
    (insert "foobar foobaz")
    (cl-letf (((symbol-function 'window-start) (lambda (&optional _) (point-min)))
              ((symbol-function 'window-end) (lambda (&optional _ _) (point-max))))
      (let ((matches (fussy-avy--find-matches "foobar")))
        (should (>= (length matches) 1))
        ;; First match should be exact (score 0)
        (should (= 0 (cdar matches)))))))

;;; Max Forgiving Mode Tests

(ert-deftest fussy-avy-test-max-forgiving-mode ()
  "Max forgiving mode sets correct values."
  (let ((fussy-avy-max-distance 2)
        (fussy-avy-min-input-length 3))
    (fussy-avy-max-forgiving)
    (should (= 4 fussy-avy-max-distance))
    (should (= 2 fussy-avy-min-input-length))))

(provide 'fussy-avy-test)
;;; fussy-avy-test.el ends here
