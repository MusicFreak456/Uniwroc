#include <stdio.h>
#include <stdbool.h>
#include <inttypes.h>
#include <assert.h>

#define BUFFER_SIZE 20

typedef uint8_t buffer_size_t;
typedef char buffer_elem_t;
typedef struct {
  buffer_size_t read_index;
  buffer_size_t write_index;
  buffer_size_t number_of_elems;
  buffer_elem_t array[BUFFER_SIZE];
} cyclic_buffer;

cyclic_buffer write_buffer;
bool write_done;

buffer_size_t next(buffer_size_t index) {
  buffer_size_t index_incr = index + 1;
  return (index_incr == BUFFER_SIZE) ? 0 : index_incr;
}

bool append(cyclic_buffer *buffer, buffer_elem_t elem) {
  if( buffer->number_of_elems == BUFFER_SIZE ) return false;

  buffer_size_t write_index = buffer->write_index;
  buffer->array[write_index] = elem;
  buffer->write_index = next(write_index);
  buffer->number_of_elems++;
  return true;
}

bool consume(cyclic_buffer *buffer, buffer_elem_t *elem) {
  if( buffer->number_of_elems == 0 ) return false;

  buffer_size_t read_index = buffer->read_index;
  *elem = buffer->array[read_index];
  buffer->read_index = next(read_index);
  buffer->number_of_elems--;
  return true;
}

int main() {
  buffer_elem_t elem;
  while (consume(&write_buffer, &elem)) {
    assert(false);
  }

  int i = 0;
  while(append(&write_buffer, i)) i++;

  for (int i = 0; i < 5; i++)
  {
    consume(&write_buffer, &elem);
    printf("%d\n", elem);
  }
  
  while(append(&write_buffer, i)) i++;

  while (consume(&write_buffer, &elem)) {
    printf("%d\n", elem);
  }

  return 0;
}