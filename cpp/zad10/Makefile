GPP = g++
CPP_FLAGS =-std=c++11 -Wall -Wextra -pedantic
NAME = program
OBJ = main.o \
Syntax.o \
Parser.o \
Operand.o \
Evaluator.o \
Function.o

program: $(OBJ)
	$(GPP) $(CPP_FLAGS) -o $(NAME) $(OBJ)

main.o: main.cpp
	$(GPP) $(CPP_FLAGS) -c -o $@ $<

%.o : %.cpp %.hpp
	$(GPP) $(CPP_FLAGS) -c -o $@ $<

clean:
	rm *.o $(NAME)