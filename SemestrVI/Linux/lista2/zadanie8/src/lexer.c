#include"lexer.h"
#include<string.h>
#include<stdlib.h>
#include<stdio.h>

int args_of_tokens(char **buff, token_t *tokens) {
  int idx = 0;

  while(tokens[idx].type != T_NULL) {
    if(tokens[idx].type != T_STRING)
      return -1;
    buff[idx] = tokens[idx].payload.str;
    idx++;
  }

  buff[idx] = NULL;
  return idx;
}

token_t token_of_string(char *str) {
  token_t t;

  if (strlen(str) == 1) {
    switch (str[0]) {
    case '&':
      t.type = T_BG;
      break;
    case '>':
      t.type = T_OUT;
      break;
    case '<':
      t.type = T_IN;
      break;
    case '|':
      t.type = T_PIPE;
      break;
    default:
      t.type = T_STRING;
      t.payload.str = str;
      break;
    }
    return t;
  }

  t.type = T_STRING;
  t.payload.str = str;

  return t;
}

token_t *tokenize(char *line, int *number_of_tokens) {
  int bufsize = LEX_STARTING_BUFSIZE;
  int token_idx = 0;

  token_t *tokens = malloc(sizeof(token_t) * (bufsize + 1));
  char *word = strtok(line, " \t");

  while (word != NULL) {
    if(token_idx == bufsize) {
      bufsize *= 2;
      tokens = realloc(tokens, sizeof(token_t) * (bufsize + 1));
    }
    tokens[token_idx] = token_of_string(word);
    token_idx++;
    word = strtok(NULL, " \t");
  }

  tokens[token_idx].type = T_NULL;
  *number_of_tokens = token_idx + 1;
  return tokens;
}
