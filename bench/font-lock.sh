#!/usr/bin/env sh

# EMACS should be set to the binary to invoke.
# FILE should be set to the file to test against.

bench() {
    echo "$EMACS -Q --batch -nw  \
-L . -L ../json-mode -L ../json-snatcher -L ../json-reformat \
--eval '(setq enable-local-variables nil)' \
--eval '(setq large-file-warning-threshold nil)' \
--eval '(switch-to-buffer (find-file-literally \"$FILE\"))' \
--eval \"$1\" \
--eval '(condition-case err \
(with-current-buffer (current-buffer) \
(setq font-lock-major-mode nil) \
(syntax-ppss-flush-cache -1) \
(font-lock-set-defaults) \
(save-excursion \
(font-lock-fontify-region (point-min) (point-max)))) \
((debug error) (kill-emacs (error-message-string err))))' \
--eval '(goto-char (point-max))' \
--eval '(kill-emacs)'"
}

hyperfine --export-markdown "$EXPORT" --show-output \
          --command-name "fundamental-mode" "$(bench "(fundamental-mode)")" \
          --command-name "prog-mode" "$(bench "(prog-mode)")" \
          --command-name "jsonian-mode" "$(bench "(progn (require 'jsonian) (jsonian-mode))")" \
          --command-name "json-mode" "$(bench "(progn (require 'json-mode) (json-mode))")" \
          --command-name "javascript-mode" "$(bench "(javascript-mode)")"
