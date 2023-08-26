
## Benchmarks

The original reason I wrote jsonian is that I needed to read and naviage very large JSON
files, and Emacs was slowing me down. To keep jsonian fast, I maintain benchmarks of
jsonian doing real world tasks.

### `font-lock`ing a large buffer

This benchmark opens a very large (42M) JSON file, then forces Emacs to fontify it. It
finally moves point to the end of the file and exits.

| Package | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `fundamental-mode` | 1.444 ± 0.174 | 1.301 | 1.734 | 1.00 ± 0.12 |
| `prog-mode` | 1.442 ± 0.039 | 1.402 | 1.488 | 1.00 |
| `jsonian-mode` | 2.296 ± 0.013 | 2.289 | 2.332 | 1.59 ± 0.04 |
| `json-mode` | 3.775 ± 0.033 | 3.762 | 3.867 | 2.62 ± 0.07 |
| `javascript-mode` | 13.599 ± 0.288 | 13.341 | 14.145 | 9.43 ± 0.32 |

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
| `jsonian-format-region` | 1.709 ± 0.091 | 1.633 | 1.877 | 1.12 ± 0.06 |
| `jsonian-format-region (minimize)` | 1.524 ± 0.010 | 1.516 | 1.549 | 1.00 |
| `json-pretty-print-buffer` | 4.582 ± 0.006 | 4.576 | 4.593 | 3.01 ± 0.02 |
| `json-pretty-print-buffer (minimize)` | 4.440 ± 0.114 | 4.384 | 4.753 | 2.91 ± 0.08 |

We see that the built-in `json-pretty-print-buffer` takes significantly longer then our
implementation.

Notes:

1. Both `jsonian` and `json-mode` were byte-compiled for the `font-lock` benchmark.
1. Tests were run against GNU Emacs 30.0.50.
1. These benchmarks were taken on an Apple M2 Max with 64GB running macOS Ventura.
