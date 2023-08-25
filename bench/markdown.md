
## Benchmarks

The original reason I wrote jsonian is that I needed to read and naviage very large JSON
files, and Emacs was slowing me down. To keep jsonian fast, I maintain benchmarks of
jsonian doing real world tasks.

### `font-lock`ing a large buffer

This benchmark opens a very large (42M) JSON file, then forces Emacs to fontify it. It
finally moves point to the end of the file and exits.

| Package | Mean [s] | Min [s] | Max [s] | Relative |
|:---|---:|---:|---:|---:|
| `fundamental-mode` | 1.351 ± 0.003 | 1.345 | 1.355 | 1.00 |
| `prog-mode` | 1.437 ± 0.030 | 1.422 | 1.520 | 1.06 ± 0.02 |
| `jsonian-mode` | 2.349 ± 0.032 | 2.316 | 2.414 | 1.74 ± 0.02 |
| `json-mode` | 3.838 ± 0.041 | 3.808 | 3.947 | 2.84 ± 0.03 |
| `javascript-mode` | 13.547 ± 0.086 | 13.498 | 13.789 | 10.03 ± 0.07 |

We can use this benchmark to derive how long different parts of the proces take.

- Fundamental mode is the lower limit. This is the time Emacs spends processing the
  buffer, parsing sexps, etc.

- We see that `prog-mode` doesn\'t do much more then `fundamental-mode`, which makes
  sense.

- Applying JSON formatting take at most `jsonian-mode` - `prog-mode`.

Notes:

- Both `jsonian` and `json-mode` were byte-compiled for the benchmark.
