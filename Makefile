EMACS = emacs

build: json-fixer.elc

test: build json-fixer-tests.elc
	$(EMACS) -Q --batch -l ert -L . -l json-fixer-tests.elc -f ert-run-tests-batch-and-exit

clean:
	rm *.elc

%.elc: %.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
