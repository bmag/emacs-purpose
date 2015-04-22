EMACS ?= emacs
CASK ?= cask
SOURCES = window-purpose-utils.el window-purpose-configuration.el window-purpose-core.el window-purpose-prefix-overload.el window-purpose-switch.el window-purpose-layout.el window-purpose-fixes.el window-purpose.el window-purpose-x.el
INPUT_FILE := "test/user-input.txt"

all: test

deps:
	${CASK} install

test: clean-elc deps
	rm -f ${INPUT_FILE}
	touch ${INPUT_FILE}
	${CASK} exec ert-runner < ${INPUT_FILE}

compile: clean-elc deps
	${CASK} exec ${EMACS} -Q --batch -L . -f batch-byte-compile ${SOURCES}

clean: clean-elc clean-deps

clean-elc:
	rm -f *.elc

clean-deps:
	rm -rf .cask/

.PHONY: all test clean
