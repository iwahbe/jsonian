EMACS = emacs
SHELL := /bin/bash

all: build

.PHONY: test lint clean

build: jsonian.elc

test: build jsonian-tests.elc
	$(EMACS) -Q --batch -l ert -L . -l jsonian-tests -l jsonian-tests.elc -l jsonian.elc -f ert-run-tests-batch-and-exit

clean:
	@# We do this so removed files are listed
	@if compgen -G "*.elc" > /dev/null; then \
	  for f in *.elc; do                     \
    echo "rm $$f" && rm $$f;               \
	  done                                   \
    fi

lint:
	$(EMACS) -Q --batch --eval '(checkdoc-file "jsonian.el")'

%.elc: %.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
