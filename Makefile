EMACS = emacs
SHELL := /bin/bash

all: build

.PHONY: test lint clean

build: jsonian.elc

test: build jsonian-tests.elc
	$(EMACS) -Q --batch -l ert -L . -l jsonian-tests.elc -l jsonian.elc -f ert-run-tests-batch-and-exit

clean:
	@# We do this so removed files are listed
	@if compgen -G "*.elc" > /dev/null; then \
	  for f in *.elc; do                     \
    echo "rm $$f" && rm $$f;               \
	  done                                   \
    fi

# Here we want run checkdoc-file, and error if it finds a lint.
lint:
	$(EMACS) -Q --batch \
    --eval '(checkdoc-file "jsonian.el")' 2> lint.log
	@cat lint.log # Display the lint to the user.
	@[ "$$(cat lint.log)" == "" ] # Error if something was written

%.elc: %.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
