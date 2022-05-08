EMACS = emacs

build: jsonian.elc

test: build jsonian-tests.elc
	$(EMACS) -Q --batch -l ert -L . -l jsonian-tests -l jsonian-tests.elc -l jsonian.elc -f ert-run-tests-batch-and-exit

clean:
	rm *.elc

%.elc: %.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
