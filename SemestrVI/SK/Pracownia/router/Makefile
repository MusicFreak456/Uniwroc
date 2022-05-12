# Cezary Świtała 316746
PROG=router
CC=gcc
CFLAGS=-std=gnu17 -Wall -Wextra
LIBS= -lrt
RM=rm -f
OBJ=$(PROG).o broadcaster.o neighbours.o distance_vector.o \
    utils.o receiver.o

default: $(PROG)

$(PROG): $(OBJ)
	$(CC) $(CFLAGS) -o $@ $^ $(LIBS)

clean:
	$(RM) *.o

distclean: clean
	$(RM) $(PROG)
