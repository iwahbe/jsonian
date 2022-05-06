EMACS = emacs

build: jsonf.elc

test: build jsonf-tests.elc
	$(EMACS) -Q --batch -l ert -L . -l jsonf-tests -l jsonf-tests.elc -l jsonf.elc -f ert-run-tests-batch-and-exit

clean:
	rm *.elc

%.elc: %.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
