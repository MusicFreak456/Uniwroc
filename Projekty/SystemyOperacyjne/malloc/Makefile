CC = gcc -g
CFLAGS = -O3 -Wall -Werror

OBJS = malloc.o memlib.o

all: malloc.o

memlib.o: memlib.c memlib.h
malloc.o: malloc.c malloc.h memlib.h

format:
	clang-format --style=file -i *.c *.h

clean:
	rm -f *~ *.o

.PHONY: all
