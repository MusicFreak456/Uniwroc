CC = gcc -g
CFLAGS = -O3 -Wall -Werror

OBJS = mdriver.o mm.o memlib.o

all: mm.o

memlib.o: memlib.c memlib.h
mm.o: mm.c mm.h memlib.h

format:
	clang-format --style=file -i *.c *.h

clean:
	rm -f *~ *.o mdriver

.PHONY: all
