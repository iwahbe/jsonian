;;; jsonl-tests.el --- Tests for jsonl.el -*- lexical-bindings: t; -*-

;; Copyright (C) 2022 Ian Wahbe

;;; Commentary:

;; Tests for jsonl.el

;;; Code:

(require 'jsonl)
(require 'ert)

(ert-deftest jsonl--display-path ()
  (should (string=
           "[\"foo\"][\"bar\"][3][2][\"buzz\"]"
           (jsonl--display-path '("foo" "bar" 3 2 "buzz")))))

(provide 'jsonl-tests)
;;; jsonl-tests.el ends here
