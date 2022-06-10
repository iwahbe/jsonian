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
    `(with-temp-buffer
       (insert-file-contents-literally ,(format "./test-assets/%s.json" file))
       (goto-char ,point)
       ,@body)))

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
    (should-not (jsonian--traverse-forward))
    (should-point 64)))

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
             (jsonian-path 75)
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

(provide 'jsonian-tests)
;;; jsonian-tests.el ends here
