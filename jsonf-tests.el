;;; jsonf-tests.el --- Tests for jsonf.el -*- lexical-bindings: t; -*-

;; Copyright (C) 2022 Ian Wahbe

;;; Commentary:

;; Tests for jsonf.el

;;; Code:

(require 'jsonf)
(require 'ert)

(ert-deftest jsonf--display-path ()
  (should (string=
           "[\"foo\"][\"bar\"][3][2][\"buzz\"]"
           (jsonf--display-path '("foo" "bar" 3 2 "buzz")))))

(provide 'jsonf-tests)
;;; jsonf-tests.el ends here
