CASK ?= cask
EMACS ?= emacs
INPUT_FILE := "test/user-input.txt"
BUTTERCUP_INPUT_FILE := "tests/user-input.txt"

all: test

# ecukes not used, so test only init
test: buttercup

unit:
	rm -f ${INPUT_FILE}
	touch ${INPUT_FILE}
	${CASK} exec ert-runner < ${INPUT_FILE}

# ecukes:
# 	${CASK} exec ecukes

buttercup:
	rm -f ${BUTTERCUP_INPUT_FILE}
	touch ${BUTTERCUP_INPUT_FILE}
	${CASK} exec buttercup -L . -L tests < ${BUTTERCUP_INPUT_FILE}

# for systems where cask is not an option. must install requirements to user
# elpa dir in this case
buttercup-nocask:
	emacs -batch -Q -L . -L tests -f package-initialize -f buttercup-run-discover

install:
	${CASK} install

compile: clean-elc
	${CASK} exec ${EMACS} -Q --batch -L . -f batch-byte-compile window-purpose*.el

clean: clean-elc clean-deps

clean-elc:
	rm -f *.elc

clean-deps:
	rm -rf .cask/

# make ARGS='--eval "(message \"run me like this\")"' run
# make ARGS='--batch --eval "(message \"or like this\")"' run
run:
	${CASK} exec ${EMACS} -Q -L . ${ARGS}

.PHONY: all test clean
