#!/usr/bin/env sh


COMPRESSED="$FILE.compressed.json"
jq -c . "$FILE" > "$COMPRESSED"

bench () {
    echo "$EMACS -Q --batch -nw \
-L . \
--eval '(setq large-file-warning-threshold nil)' \
--eval '(switch-to-buffer (find-file-literally \"$2\"))' \
--eval \"(require 'jsonian)\" \
--eval \"$1\" \
--eval '(kill-emacs)'"
}

# Run the benchmark on the full file.
full () {
    bench "$1" "$FILE"
}

# Run the benchmark on the compressed file
cmpr () {
    bench "$1" "$COMPRESSED"
}

hyperfine --export-markdown "$EXPORT" --show-output \
          --command-name "jsonian-format-region" "$(cmpr "(let ((inhibit-message t))\
                          (jsonian-format-region (point-min) (point-max)))")" \
          --command-name "jsonian-format-region (minimize)" "$(full "(let ((inhibit-message t))\
                          (jsonian-format-region (point-min) (point-max) t))")" \
          --command-name "json-pretty-print-buffer" "$(cmpr "(json-pretty-print-buffer)")" \
          --command-name "json-pretty-print-buffer (minimize)" "$(full "(json-pretty-print-buffer t)")"

rm "$COMPRESSED"
