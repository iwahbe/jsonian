#!/usr/bin/env sh


COMPRESSED="$FILE.compressed.json"
jq -c . "$FILE" > "$COMPRESSED"

bench () {
    echo "$EMACS -Q --batch -nw \
-L . \
--eval '(setq large-file-warning-threshold nil)' \
--eval '(switch-to-buffer (find-file-literally \"$COMPRESSED\"))' \
--eval \"(require 'jsonian)\" \
--eval \"$1\" \
--eval '(kill-emacs)'"
}

hyperfine --export-markdown "$EXPORT" --show-output \
          --command-name "jsonian-format-region" "$(bench "(let ((inhibit-message t))\
                          (jsonian-format-region (point-min) (point-max)))")" \
          --command-name "json-pretty-print-buffer" "$(bench "(json-pretty-print-buffer)")"

rm "$COMPRESSED"
