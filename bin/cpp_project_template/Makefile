CC=/usr/local/opt/llvm/bin/clang++
LDFLAGS=-L/usr/local/opt/llvm/lib -lc++ -lc++experimental
CPPFLAGS=-I/usr/local/opt/llvm/include/ -I/usr/local/opt/llvm/include/c++/v1/
CPPFLAGS_COMMON=-std=c++17 -stdlib=libc++ -Wall -Wextra -Werror
CFLAGS=$(LDFLAGS) $(CPPFLAGS) $(CPPFLAGS_COMMON) -DNDEBUG -O3
CFLAGS_DEBUG=$(LDFLAGS) $(CPPFLAGS) $(CPPFLAGS_COMMON) -g -DDEBUG -O0
SRC=src
BIN=bin
RM=rm -rf
CP=cp -rf

# The Cleaner
clean:
	@$(RM) $(BIN)/*

all: clean
	$(CC) $(CFLAGS) -o $(BIN)/helloworld $(SRC)/main.cpp
