GCC = gcc
NAME = shell
LDLIBS = -lreadline
OBJ_DIR = obj
SRC_DIR = src
OBJ_PATH = ./$(OBJ_DIR)/
SRC_PATH = ./$(SRC_DIR)/
OBJ = $(OBJ_DIR)/shell.o \
		  $(OBJ_DIR)/lexer.o \
			$(OBJ_DIR)/evaluator.o

$(NAME): $(OBJ_DIR) $(OBJ)
	$(GCC) $(OBJ_PATH)*.o $(LDLIBS) -o $(NAME)

$(OBJ_DIR):
	mkdir $(OBJ_DIR)

$(OBJ_PATH)%.o: $(SRC_PATH)%.c
	$(GCC) -c -o $@ $<

clean:
	rm -rf $(OBJ_DIR)
	rm $(NAME)
