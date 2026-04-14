EMACS ?= emacs

.PHONY: test compile clean

test:
	$(EMACS) -Q --batch \
	  -L . \
	  -l trx.el \
	  -l trx-jackett.el \
	  -l trx-test.el \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) -Q --batch \
	  -L . \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile trx.el trx-jackett.el

clean:
	rm -f *.elc
