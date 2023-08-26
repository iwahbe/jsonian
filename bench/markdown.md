
## Benchmarks

The original reason I wrote jsonian is that I needed to read and naviage very large JSON
files, and Emacs was slowing me down. To keep jsonian fast, I maintain benchmarks of
jsonian doing real world tasks.

### `font-lock`ing a large buffer

This benchmark opens a very large (42M) JSON file, then forces Emacs to fontify it. It
finally moves point to the end of the file and exits.

| Package | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `fundamental-mode` | 1.493 ± 0.111 | 1.313 | 1.593 | 1.05 ± 0.08 |
| `prog-mode` | 1.423 ± 0.016 | 1.406 | 1.452 | 1.00 |
| `jsonian-mode` | 2.443 ± 0.129 | 2.294 | 2.613 | 1.72 ± 0.09 |
| `json-mode` | 3.801 ± 0.145 | 3.748 | 4.213 | 2.67 ± 0.11 |
| `javascript-mode` | 13.441 ± 0.102 | 13.305 | 13.681 | 9.44 ± 0.13 |

We can use this benchmark to derive how long different parts of the proces take.

- Fundamental mode is the lower limit. This is the time Emacs spends processing the
  buffer, parsing sexps, etc.

- We see that `prog-mode` doesn\'t do much more then `fundamental-mode`, which makes
  sense.

- Applying JSON formatting take at most `jsonian-mode` - `prog-mode`.

- Parsing a javascript file is much more complicated (and thus expensive) then parsing a
  JSON file.

### Formatting a large buffer

This tests applying formatting to a very large (42M) JSON file that is compressed to
remove all whitespace. The formatted files are largely identical.

| Package | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `jsonian-format-region` | 1.711 ± 0.104 | 1.626 | 1.850 | 1.00 |
| `json-pretty-print-buffer` | 4.594 ± 0.013 | 4.578 | 4.620 | 2.69 ± 0.16 |

We see that the built-in `json-pretty-print-buffer` takes significantly longer then our
implementation.

Notes:

1. Both `jsonian` and `json-mode` were byte-compiled for the `font-lock` benchmark.
1. Tests were run against .
1. These benchmarks were taken on an Apple M2 Max with 64GB running macOS Ventura.
