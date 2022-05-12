/* Cezary Świtała 316746 */
#include"router.h"
#include<time.h>
#include<signal.h>
#include<string.h>
#include"distance_vector.h"
#include"neighbours.h"

void broadcast(union sigval value) {
  int sockfd = value.sival_int;
  notify_neigbours(sockfd);
  vector_scan();
  vector_print();
}

void init_broadcaster(int sockfd) {
  timer_t timer_id;
  int retval;

  struct sigevent se;
  se.sigev_notify = SIGEV_THREAD;
  se.sigev_notify_function = broadcast; 
  se.sigev_notify_attributes = NULL;
  se.sigev_value.sival_int = sockfd;

  retval = timer_create(CLOCK_REALTIME, &se, &timer_id);
  if(retval < 0) {
    error("timer_create", strerror(errno));
  }

  struct itimerspec interval;
  interval.it_value.tv_nsec = 1;
  interval.it_value.tv_sec = 0;
  interval.it_interval.tv_sec = TIME_INTERVAL;
  interval.it_interval.tv_nsec = 0;

  retval = timer_settime(timer_id, 0, &interval, 0);
  if(retval < 0) {
    error("timer_settime", strerror(errno));
  }
}
