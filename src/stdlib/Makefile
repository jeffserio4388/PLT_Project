
#
# Makefile for lab 3, part 1.
#

CC  = gcc
CXX = g++

INCLUDES =
CFLAGS   = -g -Wall -std=c99 $(INCLUDES)
CXXFLAGS = -g -Wall -std=c99 $(INCLUDES)

LDFLAGS = -g
LDLIBS = 

mylist-test: libmylist.a
# libstd.a: std.o
# 	ar rc libstd.a std.o
#	ranlib libstd.a

# header dependency
# std.o: std.h

libmylist.a: mylist.o
	ar rc libmylist.a mylist.o
	ranlib libmylist.a

# header dependency
mylist.o: mylist.h
mylist.o: mylist.h
.PHONY: clean
clean:
	rm -f *.o *~ a.out core libmylist.a mylist-test

.PHONY: all
all: clean libmylist.a mylist-test

