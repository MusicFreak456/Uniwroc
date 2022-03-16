#include<readline/readline.h>
#include<readline/history.h>

#include<stdbool.h>
#include<stdlib.h>
#include<unistd.h>

#include"evaluator.h"

void init() {
  signal(SIGINT, SIG_IGN);
  signal(SIGTSTP, SIG_IGN);
  signal(SIGTTIN, SIG_IGN);
  signal(SIGTTOU, SIG_IGN);

  setpgid(0, 0);
  tcsetpgrp(STDIN_FILENO, getpgrp());

  rl_initialize();
}

int main () {

  if(!isatty(STDIN_FILENO)) {
    fprintf(stderr, "Must be run in interactive mode\n");
    exit(EXIT_FAILURE);
  }

  init();

  while (true) {
    char *line = readline("$ ");

    if (line == NULL)
      break;

    if (strlen(line) > 0) {
      add_history(line);
      eval(line);
    }

    free(line);
  }

  return 0;
}
