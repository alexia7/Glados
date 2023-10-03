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

test_run:
	stack test

clean:
	stack clean

fclean: clean
	$(RM) $(NAME)

re: fclean all
