;;; jsonian-tests.el --- Tests for jsonian.el -*- lexical-bindings: t; -*-

;; Copyright (C) 2022 Ian Wahbe

;;; Commentary:

;; Tests for jsonian.el

;;; Code:

(require 'jsonian)
(require 'ert)

(ert-deftest jsonian--display-path ()
  (should (string=
           "[\"foo\"][\"bar\"][3][2][\"buzz\"]"
           (jsonian--display-path '("foo" "bar" 3 2 "buzz")))))

(ert-deftest jsonian--defun-traverse-literal ()
  (with-temp-buffer
    ;; Position the cursor at the `e'.
    (insert "\"foo\": true,") (backward-char)
    (jsonian--backward-true)
    (should (= (char-after) ?\ ))
    (should-error (jsonian--forward-true))
    (should (= (char-after) ?\ ))
    (forward-char)
    (jsonian--forward-true)
    (should (= (char-after) ?,))))

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

(provide 'jsonian-tests)
;;; jsonian-tests.el ends here
