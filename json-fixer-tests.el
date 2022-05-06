;;; json-fixer-tests.el --- Tests for json-fixer.el -*- lexical-bindings: t; -*-

;; Copyright (C) 2022 Ian Wahbe

;;; Commentary:

;; Tests for json-fixer.el

;;; Code:

(require 'jsonf)
(require 'ert)

(ert-deftest jsonf--display-path ()
  (should (string=
           "[\"foo\"][\"bar\"][3][2][\"buzz\"]"
           (jsonf--display-path '("foo" "bar" 3 2 "buzz")))))

(provide 'jsonf-tests)
;;; json-fixer-tests.el ends here
