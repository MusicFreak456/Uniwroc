GPP = g++
CPP_FLAGS = -std=c++11 -Wall -Wextra -pedantic
NAME = program
OBJ = main.o

program: $(OBJ)
	$(GPP) $(CPP_FLAGS) -o $(NAME) $(OBJ)

%.o: %.cpp
	$(GPP) -c -o $@ $<

clean:
	rm *.o $(NAME)