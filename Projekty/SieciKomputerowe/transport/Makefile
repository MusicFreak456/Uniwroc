# Cezary Świtała 316746
PROG=transport
CC=gcc
CFLAGS=-std=gnu17 -Wall -Wextra
LIBS= -lrt
RM=rm -f
OBJ=$(PROG).o window.o watchdog.o

default: $(PROG)

$(PROG): $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

clean:
	$(RM) *.o

distclean: clean
	$(RM) $(PROG)
