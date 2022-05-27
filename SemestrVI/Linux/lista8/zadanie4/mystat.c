#include<stdlib.h>
#include<stdio.h>
#include<stdbool.h>
#include<getopt.h>
#include<string.h>
#include<fcntl.h>
#include<unistd.h>
#include<string.h>
#include<errno.h>
#include<time.h>
#include<signal.h>
#include<pthread.h>
#include<limits.h>
#include<sys/queue.h>

typedef struct reading {
  int total;
  int total_idle;
  STAILQ_ENTRY(reading) readings;
} reading_s;

STAILQ_HEAD(,reading) readings_head;
pthread_mutex_t lock;

void print_help() {
  printf(
    "Sposób użycia: mystat [opcje]\n"
    "\n"
    "Opcje:\n"
    "  -p sec, --period=sec        odstęp (w sekundach) pomiędzy sprawdzeniem "
                                  "danych\n"
    "  -i sec, --interval=sec      odstęp (w sekundach) pomiędzy zapisami do "
                                  "pliku strumienia wyjściowego\n"
    "  -f file, --logfile=file     nazwa pliku z logami\n"
    "  -h, --help                  wyświetla ten komunikat\n\n"
  );
}

void reader(union sigval value) {
  FILE *procfile = (FILE*)(*(FILE**)value.sival_ptr);
  fseek(procfile, 0, SEEK_SET);

  char buffer[LINE_MAX];
  fgets(buffer, LINE_MAX, procfile);

  strtok(buffer, " ");

  int busy = 0;
  int idle = 0;

  char *token;
  for(int i = 0; i < 3; i++) {
    token = strtok(NULL, " ");
    busy += atoi(token);
  }

  token = strtok(NULL, " ");
  idle = atoi(token);

  for(int i = 0; i < 6; i++) {
    token = strtok(NULL, " ");
    busy += atoi(token);
  }

  pthread_mutex_lock(&lock);

  reading_s *new_reading = malloc(sizeof(reading_s));
  new_reading->total = busy + idle;
  new_reading->total_idle = idle;
  STAILQ_INSERT_TAIL(&readings_head, new_reading, readings);

  pthread_mutex_unlock(&lock);
}

void logger(union sigval value) {
  FILE *logfile = (FILE*)(*(FILE**)(value.sival_ptr));

  pthread_mutex_lock(&lock);

  float proc_max = 0.0;
  float proc_min = 100.0;
  float proc_acc = 0.0;
  int count = 0;

  reading_s *entry;
  reading_s *next_entry;

  int total_base;
  int idle_base;

  entry = STAILQ_FIRST(&readings_head);
  total_base = entry->total;
  idle_base = entry->total_idle;
  STAILQ_REMOVE_HEAD(&readings_head, readings);
  free(entry);
  
  for(entry = STAILQ_FIRST(&readings_head); entry != NULL; entry = next_entry) {
    
    int total_diff = entry->total - total_base;
    int idle_diff = entry->total_idle - idle_base;

    float proc = (float)(total_diff - idle_diff) / total_diff * 100.0;

    proc_acc += proc;
    proc_max = proc > proc_max ? proc : proc_max;
    proc_min = proc < proc_min ? proc : proc_min;
    count++;

    total_base = entry->total;
    idle_base = entry->total_idle;

    STAILQ_REMOVE_HEAD(&readings_head, readings);
    free(entry);
    next_entry = STAILQ_FIRST(&readings_head);
  }

  pthread_mutex_unlock(&lock);

  float proc_avg = proc_acc / count;

  time_t t;
  time(&t);

  fprintf(logfile, "[%.19s] %.2f %.2f %.2f\n", 
          ctime(&t), proc_avg, proc_max, proc_avg);
  fflush(logfile);
}

void create_periodical_function(int interval, void (*callback)(union sigval), 
                                void *arguments) {
  timer_t timer_id;
  int retval;

  struct sigevent se;
  se.sigev_notify = SIGEV_THREAD;
  se.sigev_notify_function = callback;
  se.sigev_notify_attributes = NULL;
  se.sigev_value.sival_ptr = arguments;

  retval = timer_create(CLOCK_REALTIME, &se, &timer_id);
  if(retval < 0) {
    fprintf(stderr, "timer_create: %s\n", strerror(errno));
    exit(EXIT_FAILURE);
  }

  struct itimerspec timer_spec;
  timer_spec.it_value.tv_nsec = 0;
  timer_spec.it_value.tv_sec = interval;
  timer_spec.it_interval.tv_sec = interval;
  timer_spec.it_interval.tv_nsec = 0;

  retval = timer_settime(timer_id, 0, &timer_spec, 0);
  if(retval < 0) {
    fprintf(stderr, "timer_settime: %s\n", strerror(errno));
    exit(EXIT_FAILURE);
  }
}

void init_reader(FILE **statfile, int period) {
  create_periodical_function(period, reader, (void*)statfile);
}

void init_logger(FILE **logfile, int interval) {
  create_periodical_function(interval, logger, (void*)logfile);
}

struct option long_options[] = {
  {"period", required_argument, 0, 'p'},
  {"interval", required_argument, 0, 'i'},
  {"logfile", required_argument, 0, 'f'},
  {"help", no_argument, 0, 'h'},
  {NULL,0,0,0}
};

#define DEFAULT_LOGFILE_PATH "/var/log/mystat.log"
#define STATFILE_PATH "/proc/stat"

int main(int argc, char **argv) {
  int period = 1;
  int interval = 60; 
  char *filename = calloc(strlen(DEFAULT_LOGFILE_PATH) + 1, sizeof(char));
  strcpy(filename, DEFAULT_LOGFILE_PATH);

  while (true) {
    int short_option;
    int long_option_index = 0;
    
    short_option 
      = getopt_long(argc, argv, "p:i:f:h", long_options, &long_option_index);
  
    if(short_option == -1)
      break;

    switch(short_option) {
      case 'p':
        period = atoi(optarg);
        break;
      case 'i':
        interval = atoi(optarg);
        break;
      case 'f':
        filename = realloc(filename, sizeof(char) * (strlen(optarg) + 1));
        strcpy(filename, optarg);
        break;
      case 'h':
        print_help();
        exit(EXIT_SUCCESS);
        break;
      case '?':
      default:
        print_help();
        exit(EXIT_FAILURE);
        break;
    }
  }

  if (2 * period >= interval) {
    fprintf(stderr, "interval must be more than twice as long as period");
    exit(EXIT_FAILURE);
  }

  FILE* statfile;
  FILE* logfile;

  statfile = fopen(STATFILE_PATH, "r");
  if(statfile == NULL) {
    fprintf(stderr, "fopen: %s\n", strerror(errno));
    exit(EXIT_FAILURE);
  }
  setbuf(statfile, NULL);

  logfile = fopen(filename, "a");
  if(logfile == NULL) {
    fprintf(stderr, "fopen: %s\n", strerror(errno));
    exit(EXIT_FAILURE);
  }

  STAILQ_INIT(&readings_head);
  if(pthread_mutex_init(&lock, NULL) != 0) {
    fprintf(stderr, "mutex_init: %s\n", strerror(errno));
    exit(EXIT_FAILURE);
  }

  init_reader(&statfile, period);
  init_logger(&logfile, interval);

  sigset_t sigset;
  sigemptyset(&sigset);
  sigaddset(&sigset, SIGHUP);

  sigprocmask(SIG_BLOCK, &sigset, NULL);

  while (true) {
    int sig;
    sigwait(&sigset, &sig);
    fclose(logfile);
    logfile = fopen(filename, "w");
  }

  return EXIT_SUCCESS;
}