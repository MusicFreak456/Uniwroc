# Cezary Świtała 316746
PROG=webserver
CC=g++
CPPFLAGS=-std=c++17 -O2 -Wall -Wextra #-fsanitize=address 
LDFLAGS=# -fsanitize=address
RM=rm -f
OBJ=$(PROG).o http_server.o http_mapper.o http_message_factory.o \
    http_message.o http_repository.o http_evaluator.o \
		http_type_resolver.o utils.o

default: $(PROG)

$(PROG): $(OBJ)

clean:
	$(RM) *.o

distclean: clean
	$(RM) $(PROG)
