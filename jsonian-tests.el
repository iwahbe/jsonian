;;; jsonian-tests.el --- Tests for jsonian.el -*- lexical-bindings: t; -*-

;; Copyright (C) 2022 Ian Wahbe

;;; Commentary:

;; Tests for jsonian.el

;;; Code:

(require 'jsonian)
(require 'ert)

(defun jsonian--test-with-options (path point test setups)
  "Run a test against `jsonian-mode'.

PATH is the path to the file where the test takes place.

POINT is the starting location of the point during the test.

TEST is a function to run the body of the test.

SETUPS is a list with elements of the form (condition . action).
The test will be run for each setup given that calling the
predicate on PATH return a non-nil value."
  (dolist (s setups)
    (with-temp-buffer
      (insert-file-contents-literally path)
      (when (funcall (car s) path)
        (funcall (cdr s))
        (setq indent-tabs-mode nil)
        (goto-char point)
        (funcall test)))))

(defun jsonian--is-strict-json (path)
  "Check if PATH ends in .json."
  (string-suffix-p ".json" path))

(defun jsonian--force-lock ()
  "Force a font lock on the buffer.
This function should be used only in testing."
  (setq font-lock-major-mode nil)
  (syntax-ppss-flush-cache -1)
  (font-lock-set-defaults)
  (save-excursion
    (font-lock-fontify-region (point-min) (point-max))))

(defconst jsonian--test-setups
  (list
   (cons #'jsonian--is-strict-json #'jsonian-mode)
   (cons #'jsonian--is-strict-json (lambda ()
                                     (jsonian-mode)
                                     (jsonian--force-lock)))
   (cons #'identity #'jsonian-c-mode)
   (cons #'identity (lambda ()
                      (jsonian-c-mode)
                      (jsonian--force-lock))))
  "The standard test scenarios to run against.
We run the JSONC setup for all files, but we only run the JSON
setup for strictly JSON files.")

(defmacro should-point (num)
  "Assert that NUM equals `point'."
  `(should (= (point) ,num)))

(defmacro with-file-and-point (file point &rest body)
  "Open the test file named FILE and go to POINT, then execute BODY."
  (declare (indent defun))
  `(jsonian--test-with-options
    (format "./test-assets/%s" ,file) ,point
    (lambda ()
      ,@body)
    jsonian--test-setups))

(ert-deftest jsonian--display-path ()
  (should (string=
           "[\"fo]o\"][\"bar\"][3][2][\"buzz\"]"
           (jsonian--display-path '("fo]o" "bar" 3 2 "buzz"))))
  (should (string=
           "[\"fo]o\"].bar[3][2].buzz"
           (jsonian--display-path '("fo]o" "bar" 3 2 "buzz") t))))

(ert-deftest jsonian--defun-traverse-literal ()
  (with-temp-buffer
    ;; Position the cursor at the `e'.
    (insert "\"foo\": true,") (backward-char)
    (should (= (char-before) ?e))
    (jsonian--backward-true)
    (backward-char)
    (should (= (char-after) ?\ ))
    (should-error (jsonian--forward-true))
    (should (= (char-after) ?\ ))
    (forward-char)
    (jsonian--forward-true)
    (should (= (char-after) ?,))
    ))

(ert-deftest jsonian--defun-traverse-predicate ()
  (with-temp-buffer
    (insert "867 \t\n 5309 world") (goto-char (point-min))
    (jsonian--forward-whitespace)
    (should (= (char-after) ?8))
    (jsonian--forward-number)
    (jsonian--forward-whitespace)
    (should (= (char-after) ?5))))

(ert-deftest jsonian--traverse-string ()
  ;; Simple
  (with-temp-buffer
    (insert "s\"foo\"e") (goto-char (1+ (point-min)))
    (jsonian--forward-string)
    (should (= (char-after) ?e))
    (should (= (char-before) ?\"))
    (jsonian--backward-string)
    (should (= (char-before) ?s)))
  ;; Escaped "
  (with-temp-buffer
    (insert "s\" fizz \\\" buzz \\\"fizzbuzz\"e") (goto-char (1+ (point-min)))
    (jsonian--forward-string)
    (should (= (char-after) ?e))
    (jsonian--backward-string)
    (should (= (char-before) ?s)))
  ;; Escaped \
  (with-temp-buffer
    (insert "s\" pop \\\" goes \\\\\"weazel\"e") (goto-char (1+ (point-min)))
    (jsonian--forward-string)
    (should (= (char-after) ?w))
    (jsonian--backward-string)
    (should (= (char-before) ?s))))

(ert-deftest jsonian--enclosing-item ()
  (with-file-and-point "path1.json" 75
    (jsonian--enclosing-item)
    (should (= (point) 64))
    (jsonian--enclosing-item)
    (should (= (point) 42))
    (jsonian--enclosing-item)
    (should (= (point) (point-min)))))

(ert-deftest jsonian--string-scan-back ()
  (with-temp-buffer
    ;; beginning of buffer
    (insert "\"foo\"")
    (goto-char 2)
    (should (jsonian--string-scan-back))
    (should-point (point-min))
    (insert "12345 ")
    (goto-char 8)
    (should (jsonian--string-scan-back))
    (goto-char 3)
    (should-not (jsonian--string-scan-back))))

(ert-deftest jsonian--pos-in-keyp ()
  (with-temp-buffer
    (insert "\"foo\": 3")
    (goto-char 2)
    (should (jsonian--pos-in-keyp))))

(ert-deftest jsonian--correct-starting-point ()
  (with-temp-buffer
    (insert "\"foo\":\"bar\"") (goto-char 2)
    ;; We start in the middle of a tag
    (jsonian--correct-starting-point)
    (should-point 7))
  (with-temp-buffer
    (insert "[true, false]") (goto-char 4)
    (jsonian--correct-starting-point)
    (should-point 2)
    (goto-char 10)
    (jsonian--correct-starting-point)
    (should-point 8)))

(ert-deftest jsonian--traverse-forward ()
  (with-file-and-point "path1.json" 44
    (jsonian--traverse-forward)
    (should-point 49)
    (jsonian--traverse-forward 3)
    (should-point 64)
    (should-not (jsonian--traverse-forward))))

(ert-deftest jsonian-indent-specified ()
  "Load `indent1' and indent each line.
We test that all lines are unchanged"
  (with-file-and-point "indent1.json" (point-min)
    (let ((jsonian-indentation 4)
          (file-contents (buffer-string)))
      (dotimes (l (count-lines (point-min) (point-max)))
        (jsonian-indent-line)
        (forward-line))
      (should (string= (buffer-string) file-contents))))
  (with-file-and-point "path1.json" (point-min)
    (let ((jsonian-indentation 4))
      (dotimes (l (count-lines (point-min) (point-max)))
        (jsonian-indent-line)
        (forward-line))
      (should (string= "{
    \"foo\": {
        \"bar\": 3
    },
    \"fizz\": [true, 2, \"3\", false, { \"some\": \"object\" }],
    \"thing1\": \"thing2\"
}
" (buffer-substring-no-properties (point-min) (point-max)))))))

(ert-deftest jsonian-path ()
  (with-file-and-point "path1.json" (point-min)
    (should (equal
             (jsonian-path nil 75)
             '("fizz" 4 "some")))
    (should (= (point) (point-min)))))

(ert-deftest jsonian-path-with-null ()
  (with-file-and-point "path-with-null.json" (point-min)
    (should (equal
             (jsonian-path nil 19)
             '("b")))
    (should (= (point) (point-min)))))

(ert-deftest jsonian-find-with-null ()
  "Test that we can use `jsonian-find' with null values.
This is different then `jsonian-path-with-null' because it tests
cached descent, instead of ascent."
  (with-file-and-point "path-with-null.json" (point-min)
    (should (= (jsonian-find ".b") 18))
    (should (= (jsonian-find ".a") 5))))

(ert-deftest jsonian-simple-segment ()
  "Check that we correctly identify simple segments."
  (mapc
   (lambda (x)
     (should (eq (jsonian--simple-path-segment-p (car x)) (cdr x))))
   '(
     ("foo" . t)
     ("foo:bar" . t)
     ("fizz/buz" . t)
     ("bar." . nil)
     ("has space" . nil)
     ("has\ttab" . nil)
     ("no\"quotes\"" . nil)
     ("[squares" . nil)
     ("other]" . nil))))

(ert-deftest jsonian--parse-path ()
  "Check that we can parse paths."
  (mapc (lambda (x)
          (should (equal (jsonian--parse-path (car x)) (cdr x))))
        '(("." . (""))
          ("" . ())
          (".foo.bar" . ("foo" "bar"))
          ("[\"foo\"][123][\"bar\"]" . ("foo" 123 "bar"))
          (".foo[\"fizz.buzz\"]" . ("foo" "fizz.buzz"))
          (".foo[\"$ref\"].bar" . ("foo" "$ref" "bar")))))

(ert-deftest jsonian--partial-parse-paths ()
  "Because `jsonian--parse-path' is used by interactive contexts, we must succeed on partial paths as well."
  (mapc (lambda (x)
          (should (equal (jsonian--parse-path (car x)) (cdr x))))
        '((".bar[1pos][3]"    . ("bar" "1pos" 3))
          (".bar[1pos]"       . ("bar" "1pos"))
          (".bar[1pos"        . ("bar" "1pos"))
          (".foo[1"           . ("foo" 1))
          (".foo[\"f"         . ("foo" "f"))
          (".foo[\" e"        . ("foo" " e"))
          (".foo["            . ("foo" ""))
          (".foo[\""          . ("foo" ""))
          (".foo. e"          . ("foo" "e"))
          (".foo.e "          . ("foo" "e"))
          (".foo.bar."        . ("foo" "bar" ""))
          ("[0].foo.bar."     . (0 "foo" "bar" ""))
          ("[\"0\"].foo.bar." . ("0" "foo" "bar" ""))
          (".foo[\"bar\""     . ("foo" "bar"))
          ("$ref.bar"         . ("$ref" "bar")))))

(ert-deftest jsonian--completing-boundary ()
  "Check that completing boundary works as expected.
Specifically, we need to comply with what `completion-boundaries' describes."
  (mapc (lambda (x)
          (let ((result (jsonian--completing-boundary (caar x) (cdar x))))
            (should (equal result (cdr x)))))
        '((("foo.bar"     . ".baz")    . (4 . 0))
          (("foo.bar."    . ".baz")    . (8 . 0))
          (("foo.bar."    . "")        . (8 . 0))
          ((".bar"        . "baz")     . (1 . 3))
          (("foo.bar"     . "")        . (4 . 0))
          ((".foo[\"fizz" . "buzz\"]") . (6 . 5))
          ((".foo[" . ""             ) . (5 . 0)))))

(ert-deftest jsonian--array-find-children ()
  "Check that we can find the children of arrays correctly."
  (with-file-and-point "children1.json" 7
    (should (equal
             (jsonian--find-children)
             '((1 . 58) (0 . 35))))))

(ert-deftest jsonian-indent-line ()
  (with-file-and-point "path1.json" 107
    (insert ",")
    (funcall-interactively #'newline-and-indent)
    (should (= (point) 111))))

(ert-deftest jsonian-indered-indent ()
  "Check that we correctly infer the indentation of our test files."
  (mapc (lambda (file)
          (with-file-and-point (concat file ".json") (point-min)
            (let ((file-contents (buffer-string)))
              (dotimes (l (count-lines (point-min) (point-max)))
                (jsonian-indent-line)
                (forward-line))
              (should (string= (buffer-string) file-contents))))
          )
        '("indent1" "children1" "path1")))

(ert-deftest jsonian-c-path ()
  "Check that we can get the path in JSONC files."
  (with-file-and-point "basic.jsonc" 184
    (should (equal (jsonian-path) '("fizz" "rule")))))

(ert-deftest jsonian-nested-array-path ()
  "Check that `jsonian-path' works in heavily nested arrays."
  (with-file-and-point "nested1.json" 55
    (should (equal (jsonian-path) '("arrays" 1 1 1 1 1 1 1 1 1 0)))))

(ert-deftest jsonian-nested-object-path ()
  "Check that `jsonian-path' works in heavily nested objects."
  (with-file-and-point "nested1.json" 125
    (should (equal (jsonian-path) '("objects" "1" "2" "3" "4" "5" "6" "7")))))

(ert-deftest jsonian-nested-mixed-path ()
  "Check that `jsonian-path' works within objects and arrays."
  (with-file-and-point "nested1.json" 187
    (should (equal (jsonian-path) '("mixed" 0 "1" "2" 1 1 "5" 1 "7")))))

(ert-deftest jsonian-compact-path ()
  "Check that `jsonian-path' works within compact JSON."
  (with-file-and-point "compact1.json" 36
    (should (equal (jsonian-path) '(0 "abc" 1 1 "final")))))

(ert-deftest jsonian-expanded-path ()
  "Check that `jsonian-path' works with more then normal spacing."
  (with-file-and-point "compact1.json" 91
    (should (equal (jsonian-path) '(1 "abc" 1 1 "final")))))

(provide 'jsonian-tests)
;;; jsonian-tests.el ends here
