EMACS = emacs
SHELL := /bin/bash

all: build

.PHONY: test lint clean checkdoc package-lint benchmark

build: jsonian.elc

test: build jsonian-tests.elc
	$(EMACS) -Q --batch -l ert -L . -l jsonian-tests.elc -l jsonian.elc -f ert-run-tests-batch-and-exit

clean:
	@# We do this so removed files are listed
	@if compgen -G "*.elc" > /dev/null; then \
	  for f in *.elc; do                     \
    echo "rm $$f" && rm $$f;                 \
	  done                                   \
    fi
	@rm -rf bin
	@rm -f ${LARGE_JSON_FILE}

# Here we want run checkdoc-file, and error if it finds a lint.
lint: checkdoc package-lint

package-lint:
	$(EMACS) -Q --batch                                                        \
	  --eval "(progn (setq package-user-dir \"$$(pwd)/bin\")                   \
      (require 'package)                                                     \
      (push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives) \
	    (package-initialize)                                                   \
      (unless package-archive-contents                                       \
        (package-refresh-contents))                                          \
	    (package-install 'package-lint))"                                      \
	  -f package-lint-batch-and-exit jsonian.el

checkdoc:
	$(EMACS) -Q --batch \
    --eval '(checkdoc-file "jsonian.el")' 2> lint.log
	@cat lint.log # Display the lint to the user.
	@[ "$$(cat lint.log)" == "" ] # Error if something was written

%.elc: %.el
	$(EMACS) -Q --batch -L .                       \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $<

bench = time $(EMACS) -Q -nw $(3) \
--eval '(setq enable-local-variables nil)' \
--eval '(setq large-file-warning-threshold nil)' \
--eval '(switch-to-buffer (find-file-literally "$(1)"))' \
--eval $(2) \
--eval '(condition-case err \
(with-current-buffer (current-buffer) \
(setq font-lock-major-mode nil) \
(syntax-ppss-flush-cache -1) \
(font-lock-set-defaults) \
(save-excursion \
(font-lock-fontify-region (point-min) (point-max)))) \
((debug error) (kill-emacs (error-message-string err))))' \
--eval '(goto-char (point-max))' \
--eval '(kill-emacs)'

LARGE_JSON_FILE := test-assets/large-json-file.json
${LARGE_JSON_FILE}:
	curl 'https://raw.githubusercontent.com/pulumi/pulumi-azure-native/master/provider/cmd/pulumi-resource-azure-native/schema.json' > ${LARGE_JSON_FILE}

bench-base: ${LARGE_JSON_FILE} jsonian.elc

bench-jsonian: bench-base
	$(call bench,${LARGE_JSON_FILE}, "(progn (require 'jsonian) (jsonian-mode))", -L .)

bench-json-mode: bench-base
	$(call bench,${LARGE_JSON_FILE}, "(progn (require 'json-mode) (json-mode))", -L ../json-mode -L ../json-snatcher -L ../json-reformat)

bench-javascript: bench-base
	$(call bench,${LARGE_JSON_FILE}, "(javascript-mode)",)

bench-fundamental: bench-base
	$(call bench,${LARGE_JSON_FILE},"(fundamental-mode)",)

bench-fundamental-keyword-only: bench-base
	$(call bench,${LARGE_JSON_FILE},"(progn (fundamental-mode) (setq-local font-lock-defaults '(() t)))",)

bench-prog: bench-base
	$(call bench,${LARGE_JSON_FILE},"(prog-mode)",)
