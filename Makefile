EMACS ?= emacs
mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
project_root := $(dir $(mkfile_path))

all: test

test:
	${EMACS} -q -batch -l ert -l ${project_root}test.el -f ert-run-tests-batch-and-exit
