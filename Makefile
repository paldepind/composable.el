.PHONY : all test

EMACS ?= emacs
CASK ?= cask

all: test

test:
	$(CASK) exec ecukes
