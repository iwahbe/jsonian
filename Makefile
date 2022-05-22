EMACS = emacs
SHELL = BASH

build: jsonian.elc

test: build jsonian-tests.elc
	$(EMACS) -Q --batch -l ert -L . -l jsonian-tests -l jsonian-tests.elc -l jsonian.elc -f ert-run-tests-batch-and-exit

README.md: jsonian.elc scripts/interface.el README.spec.md
	cp README.spec.md README.md
	printf "\n" >> README.md
	$(EMACS) -q --batch -L . -l scripts/interface.el -f interface >> README.md

clean:
	@# We do this so removed files are listed
	@if compgen -G "*.elc" > /dev/null; then \
	  for f in *.elc; do                     \
    echo "rm $$f" && rm $$f;               \
	  done                                   \
    fi

%.elc: %.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
