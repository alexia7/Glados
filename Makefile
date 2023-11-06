##
## EPITECH PROJECT, 2023
## Glados
## File description:
## Makefile
##

BINARY_PATH := $(shell stack path --local-install-root)
NAME = glados

all:
	stack build
	cp "$(BINARY_PATH)/bin/$(NAME)-exe" ./$(NAME)

unittest_run:
	stack test

functest_run:
	python3 test/functionnal_test.py

clean:
	stack clean

fclean: clean
	@ rm -rf *.go
	@ rm -rf Lisp/*.go
	$(RM) $(NAME)

re: fclean all
