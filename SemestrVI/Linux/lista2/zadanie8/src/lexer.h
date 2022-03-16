#ifndef LEXER_H
#define LEXER_H

#define LEX_STARTING_BUFSIZE 20

typedef enum {
  T_NULL,
  T_BG,
  T_OUT,
  T_IN,
  T_PIPE,
  T_STRING
} token_type_t;

typedef union {
  char *str;
} token_payload_t;

typedef struct {
  token_type_t type;
  token_payload_t payload;
} token_t;

token_t *tokenize(char *line, int *number_of_tokens);
int args_of_tokens(char **buff, token_t *tokens);

#endif