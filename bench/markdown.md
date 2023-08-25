
## Benchmarks

The original reason I wrote jsonian is that I needed to read and naviage very large JSON
files, and Emacs was slowing me down. To keep jsonian fast, I maintain benchmarks of
jsonian doing real world tasks.

### `font-lock`ing a large buffer

This benchmark opens a very large (42M) JSON file, then forces Emacs to fontify it. It
finally moves point to the end of the file and exits.

| Package | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `fundamental-mode` | 1.331 ± 0.003 | 1.327 | 1.337 | 1.00 |
| `prog-mode` | 1.407 ± 0.010 | 1.398 | 1.429 | 1.06 ± 0.01 |
| `jsonian-mode` | 2.280 ± 0.006 | 2.272 | 2.291 | 1.71 ± 0.01 |
| `json-mode` | 3.787 ± 0.013 | 3.766 | 3.816 | 2.84 ± 0.01 |
| `javascript-mode` | 13.466 ± 0.071 | 13.325 | 13.516 | 10.11 ± 0.06 |

We can use this benchmark to derive how long different parts of the proces take.

- Fundamental mode is the lower limit. This is the time Emacs spends processing the
  buffer, parsing sexps, etc.

- We see that `prog-mode` doesn\'t do much more then `fundamental-mode`, which makes
  sense.

- Applying JSON formatting take at most `jsonian-mode` - `prog-mode`.

### Formatting a large buffer

This tests applying formatting to a very large (42M) JSON file that is compressed to
remove all whitespace. The formatted files are largely identical.

| Package | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `jsonian-format-region` | 1.668 ± 0.014 | 1.649 | 1.701 | 1.00 |
| `json-pretty-print-buffer` | 4.637 ± 0.023 | 4.597 | 4.679 | 2.78 ± 0.03 |

We see that the built-in `json-pretty-print-buffer` takes significantly longer then our
implementation.

Notes:

- Both `jsonian` and `json-mode` were byte-compiled for the `font-lock` benchmark.
- Tests were run against GNU Emacs 30.0.50.
- These benchmarks were taken on an Apple M2 Max with 64GB running macOS Ventura.
