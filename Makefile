EMACS ?= emacs
CASK_EXEC ?= cask exec

all: test

test: clean-elc
	${CASK_EXEC} ${EMACS} -Q -batch -l purpose-test.el -f ert-run-tests-batch-and-exit

compile:
	${CASK_EXEC} ${EMACS} -Q -batch -f batch-byte-compile purpose.el

clean-elc:
	rm -f *.elc

.PHONY: all test
