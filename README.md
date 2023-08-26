# jsonian.el - JSON

`jsonian.el` provides a major mode for editing JSON files of any size. The goal is to be
feature complete against [json-mode](https://github.com/joshwnj/json-mode) with no
external dependencies or file size limits.

To that end, all functionality is guarantied to operate on arbitrarily large JSON files.
If you find a feature that works only on small files, or is slower then it should be on
large files, please file an issue.

`jsonian.el` supports standard JSON (`.json`) and JSON with comments (`.jsonc`).

## Using jsonian

### Vanilla emacs

`jsonian.el` is a single file package, and can be compiled with `make build`. Just move
the compiled `jsonian.elc` onto your load path.

### Doom emacs

If you are using [Doom Emacs](https://github.com/doomemacs/doomemacs), you can configure
doom to use `jsonian` with the following snippet.

```emacs-lisp
;;; In ~/.doom.d/packages.el
(package! jsonian :recipe (:host github :repo "iwahbe/jsonian"))
(package! json-mode :disable t)

;;; In ~/.doom.d/config.el
;; To enable jsonian to work with flycheck
(after! (jsonian flycheck) (jsonian-enable-flycheck))
;; To diasable so-long mode overrides
(after! (jsonian so-long) (jsonian-no-so-long-mode))
```

#### Vanilla emacs 27+

Clone the repository

```bash
mkdir ~/src
cd ~/src/
git clone git@github.com:iwahbe/jsonian.git
```

#### Vanilla emacs 27+

Emacs 27+ includes `so-long` mode which will supplant `jsonian-mode` if the file has any
long lines. To prevent `so-long` mode from taking over from `jsonian-mode`, call
`jsonian-no-so-long-mode` after `so-long` mode has loaded.

Initialize the local package with use-package making it work with `so-long`

```emacs-lisp
;;; In ~/.emacs.d/init-jsonian-mode.el
(use-package jsonian
  :load-path "~/src/jsonian"
  :ensure nil
  :after so-long
  :custom
  (jsonian-no-so-long-mode))
```

#### Vanilla emacs 27+ wrapped in init package

Initialize the local package with use-package making it work with
`so-long`, and also wrap it in an initialization package

```emacs-lisp
;;; In ~/.emacs.d/init.el
(require 'init-jsonian-mode)
```

Requires that `~/.emacs.d/site-elisp` (or whichever directory the
package is in) exist and be in the load path

```emacs-lisp
;;; In ~/.emacs.d/site-elisp/init-jsonian-mode.el
;;; Code:

(use-package jsonian
  :load-path "~/src/jsonian"
  :ensure nil
  :after so-long
  :custom
  (jsonian-no-so-long-mode))

(provide 'init-jsonian-mode)

;;; init-jsonian-mode.el ends here
```

## Integration with 3rd Parties

### Flycheck

[Flycheck](https://www.flycheck.org/en/latest/) integrates directly with
[json-mode](https://github.com/joshwnj/json-mode). `jsonian` provides the convenience
function `jsonian-enable-flycheck` which adds `jsonian-mode` to all checkers that support
`json-mode`. `jsonian-enable-flycheck` must run after `flycheck` has already loaded.

## Package Interface

### Functions

#### jsonian-path (&optional POS BUFFER)

Return the JSON path (as a list) of POINT in BUFFER.
It is assumed that BUFFER is entirely JSON and that the json is
valid from POS to ‘point-min’.

For example:

```json
{
  "foo": [
    {
      "bar": "█"
    },
    {
      "fizz": "buzz"
    }
  ]
}
```

with pos at █ should yield "[foo][0][bar]".

‘jsonian-path’ is optimized to work on very large json files (35 MiB+).
This optimization is achieved by
a. parsing as little of the file as necessary to find the path and
b. leveraging C code whenever possible.

By default, this command is bound to `C-c C-p`.

#### jsonian-edit-string

Edit the string at point in another buffer. The string is expanded when being edited and
collapsed back upon exit. For example, editing the string `"json\tescapes\nare\nannoying"`
will drop you into a buffer containing:

```
json	escapes
are
annoying
```

When you return from the buffer, the string is collapsed back into its escaped form
(preserving edits).

By default, this command is bound to `C-c C-s`.

#### jsonian-find

Provide an interactive completion interface for selecting an element in the
buffer. When the element is selected, jump to that point in the buffer.

By default, this command is bound to `C-c C-f`.

### jsonian-format-region

Maximize the JSON contents of the region. This is equivalent to the built-in function
`json-pretty-print`, but much faster (see "\#\# Benchmarks"). For example:

``` json
{"key":["simple",null,{"cpx": true},[]]}
```

Calling `jsonian-format-region` on the above will transform it into:

``` json
{
    "key": [
        "simple",
        null,
        {
            "cpx": true
        },
        []
    ]
}
```

If a prefix argument is supplied, `jsonian-format-region` minimizes instead of expanding.

By default, this command is bound to `C-c C-w`.

#### jsonian-enclosing-item

Move point to the enclosing node. For example:

```json
[
    { "foo": { "fizz": 3, "buzz": 5 } },
    { "bar": { "fizz": 3, "buzz": 5 } }
]
```

If the point started on the `5`, calling `jsonian-enclosing-item` would move
point to the `"` at the beginning of `"foo"`. Calling it again would move the
point to the first `{` on the second line. Calling it a final time would move
the point to the opening `[`.

By default, this function is bound to `C-c C-e`.

#### jsonian-enable-flycheck

Enable `jsonian-mode` for all checkers where `json-mode` is enabled.

<!--BENCHMARK_START-->
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
| `jsonian-format-region` | 1.709 ± 0.091 | 1.633 | 1.877 | 1.12 ± 0.06 |
| `jsonian-format-region (minimize)` | 1.524 ± 0.010 | 1.516 | 1.549 | 1.00 |
| `json-pretty-print-buffer` | 4.582 ± 0.006 | 4.576 | 4.593 | 3.01 ± 0.02 |
| `json-pretty-print-buffer (minimize)` | 4.440 ± 0.114 | 4.384 | 4.753 | 2.91 ± 0.08 |

We see that the built-in `json-pretty-print-buffer` takes significantly longer then
`jsonian-format-region`, regardless of whether we are pretty printing or minimizing.

Notes:

1. Both `jsonian` and `json-mode` were byte-compiled for the `font-lock` benchmark.
1. Tests were run against GNU Emacs 30.0.50.
1. These benchmarks were taken on an Apple M2 Max with 64GB running macOS Ventura.<!--BENCHMARK_END-->

## Contributing

Contributions are welcome, both in the form of new
[Issues](https://github.com/iwahbe/jsonian/issues/new) and PRs. Code contributors agree
that their contribution falls under the LICENSE.

## Package Status

I use `jsonian-mode` every day, and it works perfectly for my needs. I'm not aware of any
bugs at this time, nor do I think any important features are missing. Because of this, I'm
not actively adding new code to the project. The project is not abandon, and I'm happy to
address bugs or consider new feature requests.
