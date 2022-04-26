#include<stdio.h>
#include<stdlib.h>
#include<stdbool.h>
#include<getopt.h>
#include<ctype.h>
#include<unistd.h>
#include<string.h>

#define CAPITALIZE 1
#define COLOR 2
#define WORLD 4

#define IS_CAPITALIZE_ON(options) (options & CAPITALIZE)
#define IS_COLOR_ON(options) ((options & COLOR) >> 1)
#define IS_WORLD_ON(options) ((options & WORLD) >> 2)

#define ESC "\x1b["
#define ESC_CYAN ESC "36m"
#define ESC_RESET ESC "0m"

void print_help() {
  printf(
    "Sposób użycia: hwb [options] [names...]\n"
    "\n"
    "Opcje:\n"
    "  -c, -capitalize             wypisuje imiona wielką literą\n"
    "  -color=[never|auto|always]  kolorowanie imion w zależności od rozdzaju "
                                  "strumienia wyjściowego\n"
    "  -g text, -greeting=text     używa text zamiast domyślnego powitania\n"
    "  -h, -help                   wyświetla ten komunikat\n"
    "  -v, -version                wypisuje nazwę i wersję tego programu\n"
    "  -w, -world                  dodatkowo wypisuje wiersz z powitaniem" 
                                   "'world'\n\n"
  );
}

void print_version() {
  printf("hwb 0.01\nCopyright...\n");
}

void capitalize(char **names, int number_of_names) {
  for (int i = 0; i < number_of_names; i++) {
    names[i][0] = toupper(names[i][0]); 
  }
}

void print_line(char *greeting, char *name, int options) {
  bool color = IS_COLOR_ON(options);

  printf("%s, ", greeting);
  if(color) printf(ESC_CYAN);
  printf("%s", name);
  if(color) printf(ESC_RESET);
  printf("!\n");
}

void print_greetings(char *greeting, char **names, int number_of_names, 
                     int options) {
  if(IS_WORLD_ON(options)) {
    print_line(greeting, "world", options);
  }

  for(int i=0; i < number_of_names; i++) {
    print_line(greeting, names[i], options);
  }
}

struct option long_options[] = {
  {"capitalize", no_argument, 0, 'c'},
  {"color", required_argument, 0, 0},
  {"greeting", required_argument, 0, 'g'},
  {"help", no_argument, 0, 'h'},
  {"version", no_argument, 0, 'v'},
  {"world", no_argument, 0, 'w'},
  {NULL,0,0,0}
};

void process_long_options(int long_option_index, int *options) {
  switch (long_option_index) {
  case 1:
    if(strcmp(optarg, "always") == 0) {
      *options = *options | COLOR;
    } else if (strcmp(optarg, "never") == 0) {
      *options = *options & ~COLOR;
    } else if (strcmp(optarg, "auto") != 0) {
      print_help();
      exit(EXIT_FAILURE);
    }
    break;
  
  default:
    print_help();
    exit(EXIT_FAILURE);
    break;
  }
}

int main(int argc, char **argv) {
  char *greeting = malloc(sizeof(char) * (strlen("Hello") + 1));
  strcpy(greeting, "Hello");
  int options = 0;

  if(isatty(STDOUT_FILENO)) {
    options |= COLOR;
  }

  while (true) {
    int short_option;
    int long_option_index = 0;
    
    short_option 
      = getopt_long(argc, argv, "cg:hvw", long_options, &long_option_index);
  
    if(short_option == -1)
      break;

    switch(short_option) {
      case 0:
        process_long_options(long_option_index, &options);
        break;
      case 'c':
        options |= CAPITALIZE;
        break;
      case 'g':
        greeting = realloc(greeting, sizeof(char) * (strlen(optarg) + 1));
        strcpy(greeting, optarg);
        break;
      case 'h':
        print_help();
        exit(EXIT_SUCCESS);
        break;
      case 'v':
        print_version();
        exit(EXIT_SUCCESS);
        break;
      case 'w':
        options |= WORLD;
        break;
      case '?':
      default:
        print_help();
        exit(EXIT_FAILURE);
        break;
    }
  }

  char **names = argv + optind;
  int number_of_names = argc - optind;

  if(IS_CAPITALIZE_ON(options))
    capitalize(names, number_of_names);
  print_greetings(greeting, names, number_of_names, options);

  free(greeting);
  return 0;
}