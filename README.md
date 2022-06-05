# jsonian.el - JSON

`jsonian.el` provides a major mode for editing JSON files of any size. The goal is to be
feature complete against [json-mode](https://github.com/joshwnj/json-mode) with no
external dependencies or file size limits.

To that end, all functionality is guarantied to operate on arbitrarily large JSON files.
If you find a feature that works only on small files, or is slower then it should be on
large files, please file an issue.

## Using jsonian

### Vanilla emacs

`jsonian.el` is a single file package, and can be compiled with `make build`. Just move
the compiled `jsonian.elc` onto your load path.

### Doom Emacs

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

For example
{ "foo": [ { "bar": █ }, { "fizz": "buzz" } ] }
with pos at █ should yield "[foo][0][bar]".

‘jsonian-path’ is optimized to work on very large json files (35 MiB+).
This optimization is achieved by
a. parsing as little of the file as necessary to find the path and
b. leveraging C code whenever possible.

#### jsonian-edit-string

Edit the string at point in another buffer. The string is expanded when being edited and
collapsed back upon exit. For example, editing the string "json\tescapes\nare\nannoying"
will drop you into a buffer containing:

```
json	escapes
are
annoying
```

When you return from the buffer, the string is collapsed back into its escaped form
(preserving edits).

#### jsonian-enable-flycheck

Enable ‘jsonian-mode’ for all checkers where ‘json-mode’ is enabled.
