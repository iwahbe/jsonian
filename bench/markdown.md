
## Benchmarks

The original reason I wrote jsonian is that I needed to read and naviage very large JSON
files, and Emacs was slowing me down. To keep jsonian fast, I maintain benchmarks of
jsonian doing real world tasks.

### `font-lock`ing a large buffer

This benchmark opens a very large (42M) JSON file, then forces Emacs to fontify it. It
finally moves point to the end of the file and exits.

| Package | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `fundamental-mode` | 1.357 ± 0.007 | 1.346 | 1.372 | 1.00 |
| `prog-mode` | 1.431 ± 0.009 | 1.419 | 1.444 | 1.05 ± 0.01 |
| `jsonian-mode` | 2.315 ± 0.021 | 2.284 | 2.347 | 1.71 ± 0.02 |
| `json-mode` | 3.846 ± 0.062 | 3.781 | 3.992 | 2.83 ± 0.05 |
| `javascript-mode` | 13.638 ± 0.099 | 13.439 | 13.816 | 10.05 ± 0.09 |

We can use this benchmark to derive how long different parts of the proces take.

- Fundamental mode is the lower limit. This is the time Emacs spends processing the
  buffer, parsing sexps, etc.

- `prog-mode` doesn\'t do much more then `fundamental-mode`, which makes sense, since it
  takes about the same amount of time.

- Applying JSON formatting take at most `jsonian-mode` - `prog-mode`.

- Parsing a javascript file is much more complicated (and thus expensive) then parsing a
  JSON file.

### Formatting a large buffer

This tests applying formatting to a very large (42M) JSON file that is compressed to
remove all whitespace. The formatted files are largely identical.

| Package | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `jsonian-format-region` | 1.015 ± 0.011 | 1.000 | 1.034 | 1.18 ± 0.02 |
| `jsonian-format-region (minimize)` | 0.860 ± 0.007 | 0.845 | 0.869 | 1.00 |
| `json-pretty-print-buffer` | 4.655 ± 0.005 | 4.650 | 4.666 | 5.42 ± 0.04 |
| `json-pretty-print-buffer (minimize)` | 4.466 ± 0.020 | 4.437 | 4.502 | 5.20 ± 0.05 |

We see that the built-in `json-pretty-print-buffer` takes significantly longer then
`jsonian-format-region`, regardless of whether we are pretty printing or minimizing.

Notes:

1. Both `jsonian` and `json-mode` were byte-compiled for the `font-lock` benchmark.
1. Tests were run against GNU Emacs 30.0.50.
1. These benchmarks were taken on an Apple M2 Max with 64GB running macOS Ventura.
