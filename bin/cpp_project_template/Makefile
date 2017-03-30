CC=clang++
CFLAGS=-std=c++14 -Weverything -pedantic
SRC=src
BIN=bin
RM=rm -rf
CP=cp -rf

# The Cleaner
clean:
	@$(RM) $(BIN)/*

all: clean
	$(CC) $(CFLAGS) -o $(BIN)/helloworld $(SRC)/main.cpp
