# Malloc

This was one of the excercises during operating systems course at the university of Wrocław. It was written basically from scratch, under assumption that memory is no larger than 4GB. Only restriction was that we couldn't use mmap. **It was tested under lecturer's testing framework and achived total memory utilization of 95% and average of 315 cpu instructions per operation.**

This implementation is largely inspired by a description of the glibc
dynamic allocator, which I encountered while doing some CTF excercises
(this hopefuly explains the 'exploitation' part), posted here:
https://azeria-labs.com/heap-exploitation-part-2-glibc-heap-free-bins/

Major features of my implementation include:

* optimized boundary tags -- 4 bytes of header per allocated block.
* coalescing of adjecent free blocks -- lazy when block size is smaller
  then TOP_FAST_BIN and eager for larger ones.
* free bins -- linked list of free blocks is divided into 4 types
  of bins:
  * small bins -- bins containing list of blocks of exactly one size
    from 16 to 16 * SMALL_BIN_NUM bytes.
  * large bins -- bins containing sorted by size list of blocks from
    given size class. Made in such way that i-th size class will cover
    2^i different sizes.
  * fast bins -- bins for small, lazily coalesced blocks. These blocks
    are not truly freed until request for larger blocks than they can
    handle occure.
  * unsorted bin -- free will simply push coalesced blocks onto this
    list. Malloc will do the sorting.
* fast best-fit policy -- thanks to the sorting of free blocks lists,
  the one with the least padding or with no padding will be chosen.
* deferred sorting of blocks -- free does not push to any sorted bins,
  only to the unsorted ones, malloc will do the sorting when needed.
  This gives performance boost when large clusters of free operations
  occur at the end of the program.
* bitmap of used bins -- helps to traverse only those bins which have
  non-empty list of blocks.

Major flaws of my implementation:

* there are no dedicated areas for freaquently chosen/small sizes of
  blocks, this means that large number of same sized blocks may be
  assigned to non-continous chunk of memory, leaving holes between them.
* large requests are not handled by mmap, because this syscall whas
  illegal in this excercise.
