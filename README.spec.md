# jsonian.el - JSON

Provides a major mode for editing JSON files of any size. The goal is to be
feature complete against [json-mode](https://github.com/joshwnj/json-mode) with
zero non-emacs dependencies or file size limits.

To that end, all functionality is guarantied to operate on arbitrarily large
JSON files. If you find a feature that works only on small files, or is slower
then it should be on large files, please file an issue.

## Using jsonian

### Doom Emacs

If you are using [Doom Emacs](https://github.com/doomemacs/doomemacs), you can
configure doom to use `jsonian` with the following snippet.

```emacs-lisp
;;; In ~/.doom.d/packages.el
(package! json-mode :disable t)

;;; In ~/.doom.d/config.el
(use-package! jsonian
  :ensure t
  :load-path "/path/to/jsonian/jsonian.el")

;; To enable jsonian to work with flycheck
(after! (jsonian flycheck) (jsonian-enable-flycheck))
```

## Integration with 3rd Parties

### Flycheck

[Flycheck](https://www.flycheck.org/en/latest/) integrates directly with
[json-mode](https://github.com/joshwnj/json-mode). `jsonian` provides the
convenience function `jsonian-enable-flycheck` which adds `jsonian-mode` to all
checkers that support `json-mode`. `jsonian-enable-flycheck` must run after
`flycheck` has already loaded.
