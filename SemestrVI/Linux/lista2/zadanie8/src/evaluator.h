#ifndef EVALUATOR_H
#define EVALUATOR_H

#include<unistd.h>
#include<stdlib.h>
#include<stdio.h>
#include<string.h>
#include<errno.h>
#include<sys/wait.h>
#include<limits.h>
#include<fcntl.h>
#include"lexer.h"

extern char **environ;

void eval(char *line);

#endif
