CC=clang++
CFLAGS=-std=c++14 -Wall -Wextra -Wshadow -Wnon-virtual-dtor -pedantic -DNDEBUG -O3
CFLAGS_DEBUG=-std=c++14 -Wall -Wextra -Wshadow -Wnon-virtual-dtor -pedantic -g -DDEBUG -O0
SRC=src
BIN=bin
RM=rm -rf
CP=cp -rf

# The Cleaner
clean:
	@$(RM) $(BIN)/*

all: clean
	$(CC) $(CFLAGS) -o $(BIN)/helloworld $(SRC)/main.cpp
