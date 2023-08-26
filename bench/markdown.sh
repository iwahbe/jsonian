#!/usr/bin/env sh

MESSAGE=$(cat <<-EOF

## Benchmarks

The original reason I wrote jsonian is that I needed to read and naviage very large JSON
files, and Emacs was slowing me down. To keep jsonian fast, I maintain benchmarks of
jsonian doing real world tasks.

### \`font-lock\`ing a large buffer

This benchmark opens a very large (42M) JSON file, then forces Emacs to fontify it. It
finally moves point to the end of the file and exits.

$(sed 's/Command/Package/g' < bench/font-lock.md)

We can use this benchmark to derive how long different parts of the proces take.

- Fundamental mode is the lower limit. This is the time Emacs spends processing the
  buffer, parsing sexps, etc.

- \`prog-mode\` doesn\'t do much more then \`fundamental-mode\`, which makes sense, since it
  takes about the same amount of time.

- Applying JSON formatting take at most \`jsonian-mode\` - \`prog-mode\`.

- Parsing a javascript file is much more complicated (and thus expensive) then parsing a
  JSON file.

### Formatting a large buffer

This tests applying formatting to a very large (42M) JSON file that is compressed to
remove all whitespace. The formatted files are largely identical.

$(sed 's/Command/Package/g' < bench/format.md)

We see that the built-in \`json-pretty-print-buffer\` takes significantly longer then
\`jsonian-format-region\`, regardless of whether we are pretty printing or minimizing.

Notes:

1. Both \`jsonian\` and \`json-mode\` were byte-compiled for the \`font-lock\` benchmark.
1. Tests were run against $($EMACS --version | head -1).
1. These benchmarks were taken on an Apple M2 Max with 64GB running macOS Ventura.
EOF
       )

echo "$MESSAGE" > "$EXPORT"
