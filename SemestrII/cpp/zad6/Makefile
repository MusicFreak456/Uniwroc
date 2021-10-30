GPP = g++
CPP_FLAGS =-std=c++11 -Wall -Wextra -pedantic
NAME = program
OBJ = main.o Number.o Variable.o Const.o UnOperator.o BinOperator.o

program: $(OBJ)
	$(GPP) $(CPP_FLAGS) -o $(NAME) $(OBJ)

%.o : %.cpp
	$(GPP) $(CPP_FLAGS) -c -o $@ $<

clean:
	rm *.o $(NAME)