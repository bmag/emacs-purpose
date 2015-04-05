EMACS ?= emacs
CASK_EXEC ?= cask exec

all: test

deps:
	cask install

test: clean-elc deps
	${CASK_EXEC} ${EMACS} -Q --batch -L . -l window-purpose-test.el --eval '(progn (set-frame-width nil 80) (set-frame-height nil 24))' -f ert-run-tests-batch-and-exit

compile:
	${CASK_EXEC} ${EMACS} -Q --batch -f batch-byte-compile window-purpose.el

clean: clean-elc clean-deps

clean-elc:
	rm -f *.elc

clean-deps:
	rm -rf .cask/

.PHONY: all test clean
