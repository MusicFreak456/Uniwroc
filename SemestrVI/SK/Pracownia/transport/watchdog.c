#include"window.h"
#include"common.h"
#include<time.h>
#include<signal.h>
#include<string.h>

void increase_age(window_s *window, part_s *part) {
  (void)window;
  if(!part->received) {
    part->age++;
  }
}

void resend_old(window_s *window, part_s *part) {
  if(part->age >= MAX_AGE) {
    part->age = 0;
    send_part_request(window, part);
  }
}

void watchdog(union sigval value) {
  window_s *window = (window_s*) value.sival_ptr;

  pthread_mutex_lock(&window->lock);
  window_foreach(window, increase_age);
  pthread_mutex_unlock(&window->lock);
  
  pthread_mutex_lock(&window->lock);
  window_foreach(window, resend_old);
  pthread_mutex_unlock(&window->lock);
}

void init_watchdog(window_s *window) {
  timer_t timer_id;
  int retval;

  struct sigevent se;
  se.sigev_notify = SIGEV_THREAD;
  se.sigev_notify_function = watchdog; 
  se.sigev_notify_attributes = NULL;
  se.sigev_value.sival_ptr = (void*) window;

  retval = timer_create(CLOCK_REALTIME, &se, &timer_id);
  if(retval < 0) {
    error("timer_create", strerror(errno));
  }

  struct itimerspec interval;
  interval.it_value.tv_nsec = WATCHDOG_INTERVAL;
  interval.it_value.tv_sec = 0;
  interval.it_interval.tv_sec = 0;
  interval.it_interval.tv_nsec = WATCHDOG_INTERVAL;

  retval = timer_settime(timer_id, 0, &interval, 0);
  if(retval < 0) {
    error("timer_settime", strerror(errno));
  }
}