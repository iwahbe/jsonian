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

By default, this command is bound to `C-x C-p`.

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

By default, this command is bound to `C-x C-s`.

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

By default, this function is bound to `C-x C-e`.

#### jsonian-find

Provide an interactive completion interface for selecting an element in the
buffer. When the element is selected, jump to that point in the buffer.

#### jsonian-enable-flycheck

Enable `jsonian-mode` for all checkers where `json-mode` is enabled.

## Speed comparison against other modes

Part of the promise of `jsonian` is that it will be performant on large files. A
primitive benchmark is included in the `Makefile`. It opens a very very large
(42M) JSON file, and then forces emacs to fontify it. It finally moves point to
the end of the file and exits. Here is a comparison of the time it takes to
fontify the whole buffer on a file:

| Package            | Time                                                           | comparison |
| ------------------ | -------------------------------------------------------------- | ---------- |
| `fundamental-mode` | 8 seconds                                                      | 0.66       |
| `prog-mode`        | 8 seconds                                                      | 0.66       |
| `jsonian`          | 12 seconds                                                     | 1          |
| `javascript-mode`  | 31 seconds                                                     | 2.58       |
| `json-mode`        | Fails after 43 seconds with "Stack overflow in regexp matcher" | 3.58       |

Here is what we can take away from this benchmark:

- Emacs spends 8 seconds traversing the buffer and parse matching delimiters. We
  see that from the unfontified time of both `fundamental-mode` and `prog-mode`.
- `jsonian-mode` adds 4 seconds in fontification. I assume that this time is
  spent in additional regex searches and function calls.
- `javascript-mode` spends 19 seconds longer the `jsonian-mode` to achieve the
  same effect, presumably because the mode is more general. JavaScript is a much
  more complicated spec then JSON. This will result in more complicated regexes
  and functions.
- `json-mode` Spends 12 _additional_ seconds, presumably with an additional set
  of font lock regexes.

Notes:

1. Both `jsonian` and `json-mode` were byte-compiled for this benchmark. Byte
   compiling `jsonian` shaves 6 seconds off of this benchmark.
2. These benchmarks were taken on a 2.6 GHz 6-Core Intel i7 running macOS Monterey.

## Contributing

Contributions are welcome, both in the form of new
[Issues](https://github.com/iwahbe/jsonian/issues/new) and PRs. Code contributors agree
that their contribution falls under the LICENSE.

## Package Status

I use `jsonian-mode` every day, and it works perfectly for my needs. I'm not aware of any
bugs at this time, nor do I think any important features are missing. Because of this, I'm
not actively adding new code to the project. The project is not abandon, and I'm happy to
address bugs or consider new feature requests.
