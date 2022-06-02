PROGS = shell
EXTRA-CLEAN = sh-tests.*.log

include Makefile.include

CC += -fsanitize=address
LDLIBS += -lreadline

shell: shell.o command.o lexer.o jobs.o

test:
	for i in `seq 1 10`; do python3 sh-tests.py -v || exit 1; done

# vim: ts=8 sw=8 noet
