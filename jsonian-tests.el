;;; jsonian-tests.el --- Tests for jsonian.el -*- lexical-bindings: t; -*-

;; Copyright (C) 2022 Ian Wahbe

;;; Commentary:

;; Tests for jsonian.el

;;; Code:

(require 'jsonian)
(require 'ert)

(eval-when-compile
  (defmacro should-point (num)
    `(should (= (point) ,num)))
  (defmacro with-file-and-point (file point &rest body)
    "Open the test file named FILE and go to POINT."
    (declare (indent defun))
    `(progn
       (with-temp-buffer
         (insert-file-contents-literally ,(format "./test-assets/%s.json" file))
         (jsonian-mode)
         (goto-char ,point)
         ,@body)
       (with-temp-buffer
         (insert-file-contents ,(format "./test-assets/%s.json" file))
         (jsonian-mode)
         (progn ;; Force a font lock on the buffer
           (setq font-lock-major-mode nil)
           (syntax-ppss-flush-cache -1)
           (font-lock-set-defaults)
           (save-excursion
             (font-lock-fontify-region (point-min) (point-max))))
         (goto-char ,point)
         ,@body))))

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
  (with-file-and-point "path1" 75
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
  (with-file-and-point "path1" 44
    (jsonian--traverse-forward)
    (should-point 49)
    (jsonian--traverse-forward 3)
    (should-point 64)
    (should-not (jsonian--traverse-forward))))

(ert-deftest jsonian-indent-leave-alone ()
  "Load `indent1' and indent each line.
We test that all lines are unchanged"
  (with-file-and-point "indent1" (point-min)
    (let ((jsonian-spaces-per-indentation 4)
          (file-contents (buffer-string)))
      (dotimes (l (count-lines (point-min) (point-max)))
        (jsonian-indent-line))
      (should (string= (buffer-string) file-contents)))))

(ert-deftest jsonian-path ()
  (with-file-and-point "path1" (point-min)
    (should (equal
             (jsonian-path nil 75)
             '("fizz" 4 "some")))
    (should (= (point) (point-min)))))

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
          ("[\"foo\"][123][\"bar\"]" . ("foo" 123 "bar")))))

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
          (".foo[\"bar\""     . ("foo" "bar")))))

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
  (with-file-and-point "children1" 7
    (should (equal
             (jsonian--find-children)
             '((1 . 58) (0 . 35))))))

(provide 'jsonian-tests)
;;; jsonian-tests.el ends here
