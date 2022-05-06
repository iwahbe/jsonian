EMACS = emacs

build: jsonl.elc

test: build jsonl-tests.elc
	$(EMACS) -Q --batch -l ert -L . -l jsonl-tests.elc -f ert-run-tests-batch-and-exit

clean:
	rm *.elc

%.elc: %.el
	$(EMACS) -Q --batch -L . -f batch-byte-compile $<
