CC=/usr/local/opt/llvm/bin/clang++
LDFLAGS=-L/usr/local/opt/llvm/lib -lc++ -lc++experimental
CPPFLAGS=-I/usr/local/opt/llvm/include/ -I/usr/local/opt/llvm/include/c++/v1/
CFLAGS=$(LDFLAGS) $(CPPFLAGS) -std=c++17 -stdlib=libc++ -Wall -Wextra -Wshadow -pedantic -DNDEBUG -O3
CFLAGS_DEBUG=$(LDFLAGS) $(CPPFLAGS) -std=c++17 -stdlib=libc++ -g -DDEBUG -O0
SRC=src
BIN=bin
RM=rm -rf
CP=cp -rf

# The Cleaner
clean:
	@$(RM) $(BIN)/*

all: clean
	$(CC) $(CFLAGS) -o $(BIN)/helloworld $(SRC)/main.cpp
