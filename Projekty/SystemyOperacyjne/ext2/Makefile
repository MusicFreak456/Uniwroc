CC = gcc -fsanitize=address -g
CFLAGS = -Og -Wall -Wextra -Werror

all: ext2fs.o

ext2fs.o: ext2fs.c ext2fs.h ext2fs_defs.h

grade:
	./grade.py

format:
	clang-format -i *.c *.h

clean:
	rm -f *~ *.o ext2test ext2list listfs

# vim: ts=8 sw=8 noet
