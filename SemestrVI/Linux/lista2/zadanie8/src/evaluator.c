#include"evaluator.h"

void exec_command(token_t *tokens, int number_of_tokens) {
  char **args = malloc(sizeof(char*) * number_of_tokens);

  if(args_of_tokens(args, tokens)) {
    if(args[0][0] == '/'){
      execve(args[0], args, environ);
    } else {
      char absolute_path[PATH_MAX];
      char *full_path = getenv("PATH");
      char *path = strtok(full_path, ":");

      while(path != NULL) {
        strcpy(absolute_path, path);
        strcat(absolute_path, "/");
        strcat(absolute_path, args[0]);

        if (access(absolute_path, F_OK) >= 0) {
          execve(absolute_path, args, environ);
        }

        path = strtok(NULL, ":");
      }
    }
  } else {
    fprintf(stderr, "Incorrect syntax\n");
    exit(EXIT_FAILURE);
  }
  
  fprintf(stderr, "%s: %s\n", args[0], strerror(errno));
  exit(EXIT_FAILURE);
}

void maybe_close(int *fd) {
  if (*fd < 0) 
    return;
  close(*fd);
  *fd = -1;
}

int redirect(token_t *tokens, int number_of_tokens, int *input, int *output) {
  int idx = 0;

  for(int i = 0; i < number_of_tokens; i++) {
    token_t current_token = tokens[i];
    token_type_t curr_tok_type = current_token.type;

    if( curr_tok_type == T_IN || curr_tok_type == T_OUT ) {

      if( curr_tok_type == T_IN ) {
        maybe_close(input);
        *input = open(tokens[i + 1].payload.str, O_RDONLY, 0);
      } else {
        maybe_close(output);
        *output = open(tokens[i + 1].payload.str, O_CREAT | O_WRONLY, 0666);
      }

      i++;
    } else {
      tokens[idx].type = tokens[i].type;
      tokens[idx].payload = tokens[i].payload;
      idx++;
    }
  }

  return idx;
}

pid_t start_stage(pid_t pgid, int input, int output, token_t *tokens, 
                  int number_of_tokens) {
  pid_t pid = fork();

  if (pid == 0) {
    setpgid(0, pgid);
    tcsetpgrp(STDIN_FILENO, getpgid(getpid()));
    signal(SIGINT,  SIG_DFL);
    signal(SIGTSTP, SIG_DFL);
    signal(SIGTTIN, SIG_DFL);
    signal(SIGTTOU, SIG_DFL);

    if(input > 0)
      dup2(input, 0);
    if(output > 0)
      dup2(output, 1);

    maybe_close(&input);
    maybe_close(&output);

    exec_command(tokens, number_of_tokens);
  } else if (pgid) {
    setpgid(pid, pgid);
  } else {
    setpgid(pid, pid);
  }

  return pid;
}

void start_job(token_t *tokens, int number_of_tokens) {
  int status;
  int input = -1, output = -1;

  number_of_tokens = redirect(tokens, number_of_tokens, &input, &output);
  pid_t pgid = start_stage(0, input, output, tokens, number_of_tokens);

  maybe_close(&input);
  maybe_close(&output);

  tcsetpgrp(STDIN_FILENO, pgid);

  do {
    waitpid(pgid, &status, WUNTRACED);
  } while (!WIFEXITED(status) && !WIFSIGNALED(status));

  tcsetpgrp(STDIN_FILENO, getpgrp());
}

void eval(char *line) {
  int number_of_tokens;
  token_t *tokens = tokenize(line, &number_of_tokens);

  start_job(tokens, number_of_tokens);

  free(tokens);
}
