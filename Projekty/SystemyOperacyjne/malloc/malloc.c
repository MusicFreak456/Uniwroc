#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <stddef.h>
#include <unistd.h>

#include "malloc.h"
#include "memlib.h"

/* See README.md for full description */

/* --=[ helpful types ]=----------------------------------------------------- */
/*
 * Most of these types are only ment to be used as a way of
 * avoiding explicit arithmetic on void pointers.
 */

/* boundary tag declered using bit fields */
#define BOUNDARY_TAG                                                           \
  struct {                                                                     \
    uint32_t size : 29;                                                        \
    /* size of the block */                                                    \
    uint32_t bin_unsorted : 1;                                                 \
    /* indicates that block is inside unsorted bin */                          \
    uint32_t prev_allocated : 1;                                               \
    /* indicates that previous block is allocated */                           \
    uint32_t is_allocated : 1;                                                 \
    /* indicates that the block is allocated */                                \
  }
typedef BOUNDARY_TAG boundary_tag_t;

/* Most general type of block */
typedef struct {
  BOUNDARY_TAG;
  /* Usually there is a lot of padding here containing data/metadata */
} block_t;

/* Borrowed from naive version of mm.c */
typedef struct {
  BOUNDARY_TAG; // inherits boundary-tag related fields
  /*
   * Author's comment:
   * We don't know what the size of the payload will be, so we will
   * declare it as a zero-length array.  This allow us to obtain a
   * pointer to the start of the payload.
   */
  uint8_t payload[];
} allocated_block_t;

/*
 * Heap is no larger than 4GiB, so we can store block pointers
 * as 4-byte offset from the start of the heap.
 */
typedef uint32_t offset_t;

/* 0 is never a valid offset for block, so it will serve as a sentinel */
#define SENTINEL 0

typedef struct {
  BOUNDARY_TAG; // inherits boundary-tag related fields
  offset_t next_block_offset;
  offset_t prev_block_offset;
  /* There is a buddy tag at the end of the block, but there is no
   * way to express that in type (I belive), because we don't know
   * the size of the padding.
   *
   * It would look something like this:
   *  BOUNDARY_TAG;
   *  [...]
   *  uint8_t padding[???]
   *  BOUNDARY_TAG;
   */
} free_block_t;

/* --=[ Global state ]------------------------------------------------------ */

/* points to the first byte of the heap */
static void *heap_start;
/* points to the last byte after the heap */
static void *heap_end;

/* address of the first block in the heap */
static block_t *first_block;
/* address of the last block in the heap */
static block_t *last_block;
/* we need this for optimization purposes */

/* pointer to array of free bins allocated on the heap */
static offset_t *bin_array;

/* --=[ boundary tag (bt) handling ]=--------------------------------------- */

static inline void bt_set(block_t *block, size_t size, bool is_allocated,
                          bool prev_allocated) {
  block->size = size;
  block->is_allocated = is_allocated;
  block->prev_allocated = prev_allocated;
  block->bin_unsorted = false;
}

/* Given pointer to payload returns pointer to payload's block */
static inline void *bt_of_ptr(void *payload_ptr) {
  return (boundary_tag_t *)payload_ptr - 1;
}

static inline void *bt_next_block(block_t *block) {
  return (void *)block + block->size;
}

static inline block_t *bt_prev_block(block_t *block) {
  boundary_tag_t *prev_closing_tag = bt_of_ptr(block);
  return (void *)block - prev_closing_tag->size;
}

static inline void bt_set_buddy_tag(block_t *free_block) {
  block_t *next = bt_next_block(free_block);
  block_t *dummy_block = bt_of_ptr(next);
  bt_set(dummy_block, free_block->size, false, free_block->prev_allocated);
}

/* Update header and set buddy tag */
static inline void bt_free(block_t *block) {
  block->is_allocated = 0;
  bt_set_buddy_tag(block);
  if (block != last_block) {
    block_t *next = bt_next_block(block);
    next->prev_allocated = false;
  }
}

/* --=[ free list (fl) handling ]=------------------------------------------ */

/*
 * The following procedures provide abstraction for operating on double
 * linked lists inside free bins, although they don't reference them
 * so may be used independantly.
 *
 * Pointers to blocks are stored as offset from the beginning of the
 * heap, which helps to compress them to 4 bytes.
 */

/* Given offset from the start of the heap, returns pointer to block or NULL
 * if offset is specific to sentinel. */
static inline free_block_t *fl_entry_of_offset(offset_t offset) {
  if (offset == SENTINEL)
    return NULL;
  return (free_block_t *)(heap_start + offset);
}

/* Given pointer to block, returns offset to block from the start of the
 * heap, or sentinel offset if the pointer is NULL. */
static inline offset_t fl_offset_of_entry(free_block_t *entry) {
  if (entry == NULL)
    return SENTINEL;
  return (void *)entry - heap_start;
}

static inline free_block_t *fl_next(free_block_t *entry) {
  offset_t next_offset = entry->next_block_offset;
  return fl_entry_of_offset(next_offset);
}

static inline free_block_t *fl_prev(free_block_t *entry) {
  offset_t prev_offset = entry->prev_block_offset;
  return fl_entry_of_offset(prev_offset);
}

/* Covers special case when inserting block into list, but the list is empty. */
static inline void fl_init(offset_t *start, free_block_t *new_entry,
                           offset_t new_free_block_offset) {
  new_entry->next_block_offset = SENTINEL;
  new_entry->prev_block_offset = SENTINEL;
  *start = new_free_block_offset;
}

/* Push new_entry to the front of the list, which starts at *start */
static void fl_push_to_front(offset_t *start, free_block_t *new_entry) {
  offset_t new_free_block_offset = fl_offset_of_entry(new_entry);
  offset_t start_val = *start;

  if (start_val == SENTINEL) {
    /* The list is empty */
    fl_init(start, new_entry, new_free_block_offset);
    return;
  }

  /* The list is non-empty -- update pointers */
  free_block_t *first_entry = fl_entry_of_offset(start_val);
  first_entry->prev_block_offset = new_free_block_offset;
  new_entry->prev_block_offset = SENTINEL;
  new_entry->next_block_offset = start_val;
  *start = new_free_block_offset;
}

/* Push new_entry to the correct place in sorted list, which starts at *start */
static void fl_push_sorted(offset_t *start, free_block_t *new_entry) {
  offset_t new_free_block_offset = fl_offset_of_entry(new_entry);
  offset_t start_val = *start;

  if (start_val == SENTINEL) {
    /* The list is empty */
    fl_init(start, new_entry, new_free_block_offset);
    return;
  }

  /* The list is non-empty -- find the first block of greater or equal size. */
  size_t new_entry_size = new_entry->size;
  free_block_t *next = fl_entry_of_offset(start_val);
  free_block_t *prev = NULL;

  while (next != NULL && next->size < new_entry_size) {
    prev = next;
    next = fl_next(next);
  }

  if (prev == NULL) {
    /* The first block on the list was no larger. */
    next->prev_block_offset = new_free_block_offset;
    new_entry->prev_block_offset = SENTINEL;
    new_entry->next_block_offset = start_val;
    *start = new_free_block_offset;
    return;
  }

  /* Inserting in the middle or at the end. */
  prev->next_block_offset = new_free_block_offset;
  new_entry->prev_block_offset = fl_offset_of_entry(prev);
  new_entry->next_block_offset = fl_offset_of_entry(next);
  if (next != NULL) {
    /* This is not the end of the list. */
    next->prev_block_offset = new_free_block_offset;
  }
}

/* Remove entry from list which starts at *start */
static void fl_remove(offset_t *start, free_block_t *entry) {
  free_block_t *next = fl_next(entry);
  free_block_t *prev = fl_prev(entry);

  if (next) {
    next->prev_block_offset = entry->prev_block_offset;
  }

  if (prev) {
    prev->next_block_offset = entry->next_block_offset;
  } else {
    /* Removing from the front of the list */
    *start = entry->next_block_offset;
  }
}

/* --=[ miscellanous procedures ]=------------------------------------------ */

/* Round up to the nearest multiple of 16 */
static inline size_t round_up(size_t size) {
  return (size + ALIGNMENT - 1) & -ALIGNMENT;
}

/* Count the number of bits needed to store num */
static inline int bitlen_count(uint32_t num) {
  return 32 - __builtin_clz(num);
}

/* --=[ free bins (fb) handling ]=------------------------------------------ */
/*
 * Free bins:
 * T[0]  - unsorted
 * T[1]                 - T[FAST_BIN_NUM]                 -- fast  bins
 * T[FAST_BIN_NUM + 1]  - T[FAST_BIN_NUM + SMALL_BIN_NUM] -- small bins
 * T[FAST_BIN_NUM + SMALL_BIN_NUM + 1] - T[BIN_NUM - 1]   -- large bins
 * where T is the pointer to bin array.
 *
 * For description of each type of bin see the large comment at the top of
 * the file.
 * Each entry in bin array is an offset to the first block in the linked
 * list or SENTINEL if the list is empty.
 * Numbers of bins were chosen heuristically based on provided tests,
 * except for the total number of bins, which has to be calculated, such
 * that the payload of the first block will land on some address divisible
 * by 16. This is because the bin array is placed at the beginning of the
 * heap, and the first block is placed right after it.
 */

/* Total number of bins */
#define BIN_NUM 35
/* Number of fast bins */
#define FAST_BIN_NUM 5
/* Number of small bins */
#define SMALL_BIN_NUM 14
/* Index of the last bin */
#define MAX_BIN_INDX (BIN_NUM - 1)

/* Size of the largest block handled by fast bins */
#define TOP_FAST_BIN (ALIGNMENT * FAST_BIN_NUM)
/* Size of the largest block handled by small bins */
#define TOP_SMALL_BIN (ALIGNMENT * SMALL_BIN_NUM)

/* We will maintain a bit map where 1 on i'th bit means that
 * i'th bin is not empty */
static uint64_t binmap;
/* Mask to extract only the fast bin portion of the binmap */
#define FAST_BIN_MASK ((uint64_t)(((1 << FAST_BIN_NUM) - 1) << 1))
/* Mask to extract the small bin and large bin portion of the binmap */
#define SL_BIN_MASK ~(FAST_BIN_MASK | 1)

/* Given block size, calculete number of bin in which it should be put */
static inline int fb_calc_bin_num(size_t size) {
  if (size <= TOP_SMALL_BIN)
    return (size >> 4) + FAST_BIN_NUM;
  int base = size - TOP_SMALL_BIN;
  int bin = SMALL_BIN_NUM + FAST_BIN_NUM + bitlen_count(base >> 4);
  return (bin > MAX_BIN_INDX) ? MAX_BIN_INDX : bin;
}

/* Mark bin as used */
static void inline fb_mark_used(int bin) {
  binmap |= 1LLU << bin;
}

/* Mark bin as unused */
static void inline fb_mark_unused(int bin) {
  binmap &= ~(1LLU << bin);
}

/* Push block entry to according bin */
static void inline fb_push_sorted(free_block_t *entry) {
  size_t entry_size = entry->size;
  int bin = fb_calc_bin_num(entry_size);
  fl_push_sorted(&bin_array[bin], entry);
  fb_mark_used(bin);
}

/* Push block entry to unsorted bin */
static void inline fb_push_unsorted(free_block_t *entry) {
  entry->bin_unsorted = true;
  fl_push_to_front(&bin_array[0], entry);
  fb_mark_used(0);
}

/* Push block entry to according fast bin */
static void inline fb_push_fast(free_block_t *entry, size_t entry_size) {
  int bin = fb_calc_bin_num(entry_size) - FAST_BIN_NUM;
  fl_push_to_front(&bin_array[bin], entry);
  fb_mark_used(bin);
}

/* Update binmap entry if the bin is empty */
static void fb_check_is_used(int bin) {
  if (bin_array[bin] == SENTINEL)
    fb_mark_unused(bin);
}

/* Remove block from the according bin */
static void inline fb_remove(free_block_t *entry) {
  if (entry->bin_unsorted) {
    /* Block is placed in unsorted bin (T[0]) */
    entry->bin_unsorted = false;
    fl_remove(&bin_array[0], entry);
    fb_check_is_used(0);
    return;
  }
  size_t entry_size = entry->size;
  int bin = fb_calc_bin_num(entry_size);
  fl_remove(&bin_array[bin], entry);
  fb_check_is_used(bin);
}

/* --=[ mm_init ]=---------------------------------------------------------- */

int mm_init(void) {
  /* Allocate free bin table on the heap, the size
   * was chosen so first payload is at ALIGNMENT. */
  long bin_array_size = BIN_NUM * sizeof(offset_t);
  heap_start = mem_sbrk(bin_array_size);
  if ((long)heap_start < 0)
    return -1;

  bin_array = heap_start;
  /* Initialize the bin array */
  for (int i = 0; i < BIN_NUM; i++) {
    bin_array[i] = SENTINEL;
  }
  first_block = heap_start + bin_array_size;
  heap_end = last_block = first_block;
  binmap = 0;

  return 0;
}

/* --=[ free ]=------------------------------------------------------------- */

/* Coalesce adjecent free block. This does set the proper free header and
 * buddy tag on res and updates the size  */
static block_t *coalesce(block_t *block) {
  bool is_prev_free = !block->prev_allocated;
  block_t *next = bt_next_block(block);
  bool is_next_free = !(block == last_block) && !next->is_allocated;

  block_t *res = block;
  if (is_next_free) {
    /* Coalescing with the block on the right */
    fb_remove((free_block_t *)next);
    res->size += next->size;

    /* Maintain the last block pointer */
    if (next == last_block)
      last_block = res;
  }

  if (is_prev_free) {
    /* Coalescing with the block on the left */
    block_t *prev = bt_prev_block(block);
    fb_remove((free_block_t *)prev);
    prev->size += res->size;

    /* Maintain the last block pointer */
    if (res == last_block)
      last_block = prev;
    res = prev;
  }

  /* Update header and buddy tag */
  bt_free(res);
  return res;
}

void free(void *ptr) {
  if (ptr == NULL)
    return;
  block_t *block = bt_of_ptr(ptr);
  size_t block_size = block->size;

  if (block_size <= TOP_FAST_BIN) {
    /* Lazy coalescing */
    fb_push_fast((free_block_t *)block, block_size);
    return;
  }

  block = coalesce(block);
  fb_push_unsorted((free_block_t *)block);
}

/* --=[ malloc ]=----------------------------------------------------------- */

/* Search the list starting from block pointed by offset *start. Return
 * pointer to block that's first of larger or equal size or NULL if not
 * found. Since we are only searching through sorted lists, this will
 * yield best fitted results. */
static inline free_block_t *first_fit(offset_t *start, size_t size) {
  free_block_t *current_block = fl_entry_of_offset(*start);
  while (current_block != NULL && current_block->size < size) {
    current_block = fl_next(current_block);
  }
  return current_block;
}

static inline free_block_t *search_sorted_bins(size_t size) {
  uint64_t sorted_binmap = binmap & SL_BIN_MASK;

  /* Get the index of first non-empty bin containing blocks of size
   * larger or equal to the size requested */
  int starting_offset = fb_calc_bin_num(size);
  sorted_binmap >>= starting_offset;
  int next = __builtin_ffsll(sorted_binmap);
  int bin = next + starting_offset - 1;

  free_block_t *res = NULL;

  /* Search through non-empty bins until fitting free block is found */
  while (res == NULL && next) {
    res = first_fit(&bin_array[bin], size);

    sorted_binmap >>= next;
    next = __builtin_ffsll(sorted_binmap);
    bin += next;
  }
  return res;
}

/* Push each entry in unsorted bin into sorted bins and return NULL, or return
 * a pointer to first perfect fitted free block of requested size, if found. */
static inline free_block_t *resolve_unsorted_bin(size_t size) {
  offset_t curr_offset = bin_array[0];
  while (curr_offset != SENTINEL) {
    free_block_t *free_block = fl_entry_of_offset(curr_offset);
    if (free_block->size == size)
      return free_block;
    fb_remove(free_block);
    fb_push_sorted(free_block);
    curr_offset = bin_array[0];
  }
  return NULL;
}

/* Check if according fast bin has any blocks, if it does return pointer to
 * it, else return NULL. */
static inline free_block_t *check_fast_bin(size_t size) {
  int bin = fb_calc_bin_num(size) - FAST_BIN_NUM;
  offset_t entry_offset = bin_array[bin];
  if (entry_offset != SENTINEL) {
    /* Block found, return it */
    free_block_t *res = fl_entry_of_offset(entry_offset);
    fl_remove(&bin_array[bin], res);
    fb_check_is_used(bin);
    return res;
  }
  return NULL;
}

/* Coalesce lazily coalesced blocks and push them into unsorted bin */
static inline void resolve_fast_bin(offset_t *start) {
  while (*start != SENTINEL) {
    free_block_t *block = fl_entry_of_offset(*start);
    fl_remove(start, block);
    block = (free_block_t *)coalesce((block_t *)block);
    fb_push_unsorted(block);
  }
}

/* Call resolve_fast_bin for each non-empty fast bin */
static void inline resolve_fast_bins() {
  long fast_binmap = (long)(binmap & FAST_BIN_MASK);
  fast_binmap >>= 1;
  int next = __builtin_ffsl(fast_binmap);
  int bin = next;

  while (next) {
    resolve_fast_bin(&bin_array[bin]);
    fb_mark_unused(bin);
    fast_binmap >>= next;
    next = __builtin_ffsl(fast_binmap);
    bin += next;
  }
}

/* Allocate block of new size under given free block pointer. Split it into
 * two blocks if it's possible
 *
 * WARNING: This procedure is quite dangerous. For performance reasons it
 * operates under assumption that adjecent block on the right wasn't free
 * which might not be true in all cases (for example in realloc).
 * Use with caution.
 */
static inline void split_alloc(block_t *block, size_t new_block_size,
                               bool prev_allocated) {
  size_t block_size = block->size;
  bool is_last = block == last_block;
  block_t *next = NULL;
  size_t next_block_size = block_size - new_block_size;

  if (!block->is_allocated)
    fb_remove((free_block_t *)block);

  if (block_size == new_block_size || next_block_size < ALIGNMENT) {
    /* Block is already the size requested or can't be splitted in two */
    block->is_allocated = 1;
    if (!is_last) {
      /* Update next block's prev_allocated bit accordingly */
      next = bt_next_block(block);
      next->prev_allocated = true;
    }
    return;
  }

  /* Block can be split in two */
  bt_set(block, new_block_size, true, prev_allocated);
  next = bt_next_block(block);
  bt_set(next, next_block_size, false, true);
  bt_set_buddy_tag(next);
  fb_push_sorted((free_block_t *)next);

  /* Maintain the last block pointer */
  if (is_last)
    last_block = next;
}

static inline void *expand_heap(size_t bytes_num) {
  void *res = mem_sbrk(bytes_num);
  if ((long)res < 0)
    return NULL;
  heap_end += bytes_num;
  return res;
}

/* Allocate block of a given size at the end of the heap */
static block_t *expand_alloc(size_t new_block_size) {
  size_t bytes_to_alloc = new_block_size;
  block_t *block = NULL;

  if (!last_block->is_allocated && last_block != first_block) {
    /* Last block is free, so expand it by the size diffrence */
    fb_remove((free_block_t *)last_block);
    bytes_to_alloc -= last_block->size;
    block = last_block;
  }

  /* Last block is used, so expand the heap */
  void *prev_brk = expand_heap(bytes_to_alloc);
  if (block == NULL)
    block = prev_brk;

  /* Maintain the last block pointer and set header */
  last_block = block;
  bt_set(block, new_block_size, true, true);

  return block;
}

/*
 * Malloc control flow is as follows:
 * * If block size is lesser or equal to TOP_FAST_BIN check the corect
 *   fast bin and immidietely return if block was found.
 *
 * * If requested block size is larger than TOP_FAST_BIN -- free and
 *   coalesce the fast bins entries.
 *
 * * Remove entries from unsorted bin by putting them into sorted bins
 *   until perfect match is found or the bin is empty.
 *
 * * If fitting free block is not yet found, search the sorted bins.
 *
 * * If free block is still not found, allocate at the end of the heap.
 *
 * * Else allocate requested size inside the free block found (or created)
 *   and possibly split it into two blocks.
 */
void *malloc(size_t requested_size) {
  size_t block_size = round_up(sizeof(block_t) + requested_size);
  block_t *block = NULL;

  if (block_size <= TOP_FAST_BIN) {
    block = (block_t *)check_fast_bin(block_size);
    if (block)
      return ((allocated_block_t *)block)->payload;
  } else {
    resolve_fast_bins();
  }

  block = (block_t *)resolve_unsorted_bin(block_size);

  if (!block)
    block = (block_t *)search_sorted_bins(block_size);

  if (block) {
    split_alloc(block, block_size, true);
  } else {
    block = expand_alloc(block_size);
  }

  return ((allocated_block_t *)block)->payload;
}

/* --=[ realloc ]=---------------------------------------------------------- */
void *realloc(void *old_ptr, size_t requested_size) {
  /* If size == 0 then this is just free, and we return NULL. */
  if (requested_size == 0) {
    free(old_ptr);
    return NULL;
  }

  /* If old_ptr is NULL, then this is just malloc. */
  if (!old_ptr)
    return malloc(requested_size);
  /* Else this is realloc... */

  block_t *block = bt_of_ptr(old_ptr);
  size_t old_size = block->size;
  size_t new_block_size = round_up(sizeof(block_t) + requested_size);

  if (old_size == new_block_size) {
    /* Size doesn't have to change */
    return old_ptr;
  }

  if (old_size > new_block_size) {
    /* We are shrinking the block */
    split_alloc(block, new_block_size, block->prev_allocated);
    block_t *next = bt_next_block(block);
    if (!next->is_allocated) {
      fb_remove((free_block_t *)next);
      next = coalesce(next);
      fb_push_unsorted((free_block_t *)next);
    }
    return old_ptr;
  }

  /* From now on we can assume:*/
  /* old_size < new_block_size */

  if (block == last_block) {
    /* We are trying to expand the last block */
    size_t bytes_to_alloc = new_block_size - old_size;
    expand_heap(bytes_to_alloc);
    block->size = new_block_size;
    return old_ptr;
  }

  /* From now on we can assume:*/
  /* old_size < new_block_size && block != last_block */

  block_t *next = bt_next_block(block);
  size_t joined_size = next->size + block->size;
  if (!next->is_allocated && joined_size >= new_block_size) {
    /* There is enough space to the right to expand the block */
    fb_remove((free_block_t *)next);
    block->size = joined_size;

    /* Maintain the last block pointer */
    if (next == last_block)
      last_block = block;

    split_alloc(block, new_block_size, block->prev_allocated);
    return old_ptr;
  }

  /* Give up and just call malloc */
  void *new_ptr = malloc(requested_size);
  /* If malloc() fails, the original block is left untouched. */
  if (!new_ptr)
    return NULL;

  /* Copy only the requested bytes */
  if (requested_size < old_size)
    old_size = requested_size;
  memcpy(new_ptr, old_ptr, old_size);
  free(old_ptr);

  return new_ptr;
}

/* --=[ calloc ]=----------------------------------------------------------- */
void *calloc(size_t nmemb, size_t size) {
  size_t bytes = nmemb * size;
  void *new_ptr = malloc(bytes);

  /* If malloc() fails, skip zeroing out the memory. */
  if (new_ptr)
    memset(new_ptr, 0, bytes);

  return new_ptr;
}

/* --=[ mm_checkheap ]=----------------------------------------------------- */

#define newline() putc('\n', stdout);
typedef void (*action_t)(block_t *, int);

static void print_heap_block(block_t *block, int verbose) {
  newline();
  printf("Block: %p\n", block);
  printf("Size: %u \n", (uint32_t)block->size);
  printf("F: %s\n", block->is_allocated ? "no" : "yes");
  printf("P: %s\n", block->prev_allocated ? "yes" : "no");
  if (verbose > 2 && !block->is_allocated) {
    block_t *dummy = bt_of_ptr(bt_next_block(block));
    printf("(B) Size: %u \n", dummy->size);
    printf("(B) F: %s\n", dummy->is_allocated ? "no" : "yes");
    printf("(B) P: %s\n", dummy->prev_allocated ? "yes" : "no");
  }
}

static void check_adjecent_free(block_t *block, int verbose) {
  if (!block->is_allocated && block != last_block) {
    block_t *next = bt_next_block(block);
    assert(next->is_allocated);
  }
}

static void check_buddy_tag(block_t *block, int verbose) {
  if (!block->is_allocated) {
    block_t *dummy = bt_of_ptr(bt_next_block(block));
    assert(dummy->is_allocated == block->is_allocated);
    assert(dummy->size == block->size);
  }
}

static void foreach_block(action_t action, int verbose) {
  block_t *block = first_block;
  while (block != heap_end) {
    action(block, verbose);
    block = bt_next_block(block);
  }
}

static void foreach_fl_entry(action_t action, int verbose, offset_t start) {
  free_block_t *free_block = fl_entry_of_offset(start);
  while (free_block != NULL) {
    action((block_t *)free_block, verbose);
    free_block = fl_next(free_block);
  }
}

static void foreach_block_in_bins(action_t action, int verbose) {
  for (int i = 0; i < BIN_NUM; i++) {
    printf("BIN[%d]:\n", i);
    foreach_fl_entry(action, verbose, bin_array[i]);
  }
}

void mm_checkheap(int verbose) {
  /* verbose flag is used here to choose what to print, not how much to print,
   * it was just more convenient */
  switch (verbose) {
    case 2:
    case 3:
      printf("\nHeap print (cause I am that desperate): \n");
      foreach_block(print_heap_block, verbose);
      break;
    case 4:
      printf("\nTraverse bins:\n");
      foreach_block_in_bins(print_heap_block, 2);
      break;
    default:
      break;
  }
  foreach_block(check_buddy_tag, verbose);
  foreach_block(check_adjecent_free, verbose);
}
