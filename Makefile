EMACS ?= emacs
CASK_EXEC ?= cask exec

all: test

test: clean-elc
	${CASK_EXEC} ${EMACS} -Q --batch -L . -l purpose-test.el --eval '(progn (set-frame-width nil 80) (set-frame-height nil 24))' -f ert-run-tests-batch-and-exit

compile:
	${CASK_EXEC} ${EMACS} -Q --batch -f batch-byte-compile purpose.el

clean-elc:
	rm -f *.elc

.PHONY: all test
