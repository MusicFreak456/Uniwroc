# Cezary Świtała 316746
PROG=traceroute
CC=gcc
CFLAGS=-std=gnu17 -Wall -Wextra
RM=rm -f
OBJ=$(PROG).o icmp_checksum.o parse_address.o sender.o receiver.o

default: $(PROG)

$(PROG): $(OBJ)

clean:
	$(RM) *.o

distclean: clean
	$(RM) $(PROG)
