CASK ?= cask
EMACS ?= emacs
INPUT_FILE := "test/user-input.txt"

all: test

# ecukes not used, so test only init
test: unit

unit:
	${CASK} exec ert-runner < ${INPUT_FILE}

ecukes:
	${CASK} exec ecukes

install:
	${CASK} install
