EMACS ?= emacs
CASK ?= cask
SOURCES = window-purpose-utils.el window-purpose-configuration.el window-purpose-core.el window-purpose-prefix-overload.el window-purpose-switch.el window-purpose-layout.el window-purpose-fixes.el window-purpose.el window-purpose-x.el

all: test

deps:
	${CASK} install

test: clean deps
	${CASK} exec ${EMACS} -Q --batch -L . -l window-purpose-test.el --eval '(progn (set-frame-width nil 80) (set-frame-height nil 24))' -f ert-run-tests-batch-and-exit

compile: clean-elc deps
	echo ${EMACS}
	${CASK} exec ${EMACS} -Q --batch -L . -f batch-byte-compile ${SOURCES}

clean: clean-elc clean-deps

clean-elc:
	rm -f *.elc

clean-deps:
	rm -rf .cask/

.PHONY: all test clean
