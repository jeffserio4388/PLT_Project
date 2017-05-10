
CC  = gcc
CXX = g++

INCLUDES =
CFLAGS   = -g -Wall -std=c99 $(INCLUDES)
CXXFLAGS = -g -Wall -std=c99 $(INCLUDES)

LDFLAGS = -g
LDLIBS = 


libmylist.a: mylist.o
	ar rc libmylist.a mylist.o
	ranlib libmylist.a

# header dependency
mylist.o: mylist.h
#mylist.o: mylist.h


.PHONY: clean
clean:
	rm -f *.o *~ a.out core libmylist.a 

.PHONY: all
all: clean libmylist.a

