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

(ert-deftest jsonian-path ()
  (with-file-and-point "path1" (point-min)
    (should (equal
             (jsonian-path 75)
             '("fizz" 4 "some")))
    (should (= (point) (point-min)))))

(ert-deftest jsonian--display-path ()
  (should (string=
           "[\"foo\"][\"bar\"][3][2][\"buzz\"]"
           (jsonian--display-path '("foo" "bar" 3 2 "buzz")))))

(ert-deftest jsonian--defun-traverse-literal ()
  (with-temp-buffer
    ;; Position the cursor at the `e'.
    (insert "\"foo\": true,") (backward-char 2)
    (should (= (char-after) ?e))
    (jsonian--backward-true)
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
    (backward-char)
    (jsonian--backward-string)
    (should (= (char-after) ?s)))
  ;; Escaped "
  (with-temp-buffer
    (insert "s\" fizz \\\" buzz \\\"fizzbuzz\"e") (goto-char (1+ (point-min)))
    (jsonian--forward-string)
    (should (= (char-after) ?e))
    (backward-char)
    (jsonian--backward-string)
    (should (= (char-after) ?s)))
  ;; Escaped \
  (with-temp-buffer
    (insert "s\" pop \\\" goes \\\\\"weazel\"e") (goto-char (1+ (point-min)))
    (jsonian--forward-string)
    (should (= (char-after) ?w))
    (backward-char)
    (jsonian--backward-string)
    (should (= (char-after) ?s))))

(ert-deftest jsonian--enclosing-item ()
  (with-file-and-point "path1" 75
    (jsonian--enclosing-item)
    (should (= (point) 64))
    (jsonian--enclosing-item)
    (should (= (point) 34))
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
    (should-point 6))
  (with-temp-buffer
    (insert "[true, false]") (goto-char 3)
    (jsonian--correct-starting-point)
    (should-point 6)
    (goto-char 10)
    (jsonian--correct-starting-point)
    (should-point 13)))

(ert-deftest jsonian--traverse-forward ()
  (with-file-and-point "path1" 44
    (jsonian--traverse-forward)
    (should-point 49)
    (jsonian--traverse-forward 3)
    (should-point 64)
    (should-not (jsonian--traverse-forward))
    (should-point 64)
    ))

(provide 'jsonian-tests)
;;; jsonian-tests.el ends here
