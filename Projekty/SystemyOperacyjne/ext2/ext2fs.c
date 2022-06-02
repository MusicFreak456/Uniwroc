#define L1_SIZE BLK_POINTERS
#define L2_SIZE (L1_SIZE * BLK_POINTERS)
#define L3_SIZE (L2_SIZE * BLK_POINTERS)
#define DP_UPPER_BOUND EXT2_NDADDR
/* Index upper bounds for indirect block pointer trees */
#define L1_UPPER_BOUND (DP_UPPER_BOUND + L1_SIZE)
#define L2_UPPER_BOUND (L1_UPPER_BOUND + L2_SIZE)
#define L3_UPPER_BOUND (L2_UPPER_BOUND + L3_SIZE)

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <stdalign.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdnoreturn.h>
#include <string.h>
#include <unistd.h>

#include "ext2fs_defs.h"
#include "ext2fs.h"

/* If you want debugging output, use the following macro.  When you hand
 * in, remove the #define DEBUG line. */
#undef DEBUG
#ifdef DEBUG
#define debug(...) printf(__VA_ARGS__)
#else
#define debug(...)
#endif

/* Call this function when an unfixable error has happened. */
static noreturn void panic(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  vfprintf(stderr, fmt, ap);
  fputc('\n', stderr);
  va_end(ap);
  exit(EXIT_FAILURE);
}

/* Number of lists containing buffered blocks. */
#define NBUCKETS 16

/* Since majority of files in a filesystem are small, `idx` values will be
 * usually low. Since ext2fs tends to allocate blocks at the beginning of each
 * block group, `ino` values are less predictable. */
#define BUCKET(ino, idx) (((ino) + (idx)) % NBUCKETS)

/* That should give us around 64kB worth of buffers. */
#define NBLOCKS (NBUCKETS * 4)

/* Structure that is used to manage buffer of single block. */
typedef struct blk {
  TAILQ_ENTRY(blk) b_hash;
  TAILQ_ENTRY(blk) b_link;
  uint32_t b_blkaddr; /* block address on the block device */
  uint32_t b_inode;   /* i-node number of file this buffer refers to */
  uint32_t b_index;   /* block index from the beginning of file */
  uint32_t b_refcnt;  /* if zero then block can be reused */
  void *b_data;       /* raw data from this buffer */
} blk_t;

typedef TAILQ_HEAD(blk_list, blk) blk_list_t;

/* BLK_ZERO is a special value that reflect the fact that block 0 may be used to
 * represent a block filled with zeros. You must not dereference the value! */
#define BLK_ZERO ((blk_t *)-1L)

/* All memory for buffers and buffer management is allocated statically.
 * Using malloc for these would introduce unnecessary complexity. */
static alignas(BLKSIZE) char blkdata[NBLOCKS][BLKSIZE];
static blk_t blocks[NBLOCKS];
static blk_list_t buckets[NBUCKETS]; /* all blocks with valid data */
static blk_list_t lrulst;            /* free blocks with valid data */
static blk_list_t freelst;           /* free blocks that are empty */

/* File descriptor that refers to ext2 filesystem image. */
static int fd_ext2 = -1;

/* How many i-nodes fit into one block? */
#define BLK_INODES (BLKSIZE / sizeof(ext2_inode_t))

/* How many block pointers fit into one block? */
#define BLK_POINTERS (BLKSIZE / sizeof(uint32_t))

/* Properties extracted from a superblock and block group descriptors. */
static size_t inodes_per_group;      /* number of i-nodes in block group */
static size_t blocks_per_group;      /* number of blocks in block group */
static size_t group_desc_count;      /* numbre of block group descriptors */
static size_t block_count;           /* number of blocks in the filesystem */
static size_t inode_count;           /* number of i-nodes in the filesystem */
static size_t first_data_block;      /* first block managed by block bitmap */
static ext2_groupdesc_t *group_desc; /* block group descriptors in memory */

/*
 * Buffering routines.
 */

/* Opens filesystem image file and initializes block buffers. */
static int blk_init(const char *fspath) {
  if ((fd_ext2 = open(fspath, O_RDONLY)) < 0)
    return errno;

  /* Initialize list structures. */
  TAILQ_INIT(&lrulst);
  TAILQ_INIT(&freelst);
  for (int i = 0; i < NBUCKETS; i++)
    TAILQ_INIT(&buckets[i]);

  /* Initialize all blocks and put them on free list. */
  for (int i = 0; i < NBLOCKS; i++) {
    blocks[i].b_data = blkdata[i];
    TAILQ_INSERT_TAIL(&freelst, &blocks[i], b_link);
  }

  return 0;
}

/* Allocates new block buffer. */
static blk_t *blk_alloc(void) {
  blk_t *blk = NULL;

  /* Initially every empty block is on free list. */
  if (!TAILQ_EMPTY(&freelst)) {
    /* freelst in non-emty, so we take the first block, remove it from
     * freelst, and return
     */
    blk = TAILQ_FIRST(&freelst);
    TAILQ_REMOVE(&freelst, blk, b_link);

    debug("freelst hit: %p\n", blk);
    return blk;
  }

  /* Eventually free list will become exhausted.
   * Then we'll take the last recently used entry from LRU list. */
  if (!TAILQ_EMPTY(&lrulst)) {
    blk = TAILQ_FIRST(&lrulst);
    TAILQ_REMOVE(&lrulst, blk, b_link);
    /* Block may contain data from previous allocation, so we fill
     * it with zeros */
    memset(blk->b_data, 0, BLKSIZE);

    /* Block is still present in it's bucket and it has to be removed */
    uint32_t inode = blk->b_inode;
    uint32_t block_index = blk->b_index;
    blk_list_t *bucket = &buckets[BUCKET(inode, block_index)];
    TAILQ_REMOVE(bucket, blk, b_hash);

    debug("lrulst hit: %p\n", blk);
    return blk;
  }

  /* No buffers!? Have you forgot to release some? */
  panic("Free buffers pool exhausted!");
}

/* Acquires a block buffer for file identified by `ino` i-node and block index
 * `idx`. When `ino` is zero the buffer refers to filesystem metadata (i.e.
 * superblock, block group descriptors, block & i-node bitmap, etc.) and `off`
 * offset is given from the start of block device. */
static blk_t *blk_get(uint32_t ino, uint32_t idx) {
  blk_list_t *bucket = &buckets[BUCKET(ino, idx)];
  blk_t *blk = NULL;

  /* Locate a block in the buffer and return it if found. */
  debug("\nblk_get(%d, %d)\n", ino, idx);
  /* Traverse the bucket until block with the given inode and block index
   * is found. */
  TAILQ_FOREACH (blk, bucket, b_hash) {
    if (blk->b_inode != ino || blk->b_index != idx)
      continue;

    debug("bucket hit: %p\n", blk);

    if (blk->b_refcnt == 0) {
      /* If the refrence count is zero, the the block was already placed in
       * lrulst, so it has to be removed */
      TAILQ_REMOVE(&lrulst, blk, b_link);
    }
    blk->b_refcnt++;

    return blk;
  }

  long blkaddr = ext2_blkaddr_read(ino, idx);
  debug("ext2_blkaddr_read(%d, %d) -> %ld\n", ino, idx, blkaddr);
  if (blkaddr == -1)
    return NULL;
  if (blkaddr == 0)
    return BLK_ZERO;
  if (ino > 0 && !ext2_block_used(blkaddr))
    panic("Attempt to read block %d that is not in use!", blkaddr);

  blk = blk_alloc();
  blk->b_inode = ino;
  blk->b_index = idx;
  blk->b_blkaddr = blkaddr;
  blk->b_refcnt = 1;

  ssize_t nread =
    pread(fd_ext2, blk->b_data, BLKSIZE, blk->b_blkaddr * BLKSIZE);
  if (nread != BLKSIZE)
    panic("Attempt to read past the end of filesystem!");

  TAILQ_INSERT_HEAD(bucket, blk, b_hash);
  return blk;
}

/* Releases a block buffer. If reference counter hits 0 the buffer can be
 * reused to cache another block. The buffer is put at the beginning of LRU list
 * of unused blocks. */
static void blk_put(blk_t *blk) {
  if (--blk->b_refcnt > 0)
    return;

  TAILQ_INSERT_HEAD(&lrulst, blk, b_link);
}

/*
 * Ext2 filesystem routines.
 */

/* Reads block bitmap entry for `blkaddr`. Returns 0 if the block is free,
 * 1 if it's in use, and EINVAL if `blkaddr` is out of range. */
int ext2_block_used(uint32_t blkaddr) {
  if (blkaddr >= block_count)
    return EINVAL;
  int used = 0;
  /* Retrive block bitmap from block's group descriptor */
  uint32_t relative_address = blkaddr - first_data_block;
  uint32_t block_group = relative_address / blocks_per_group;
  uint32_t block_index = relative_address % blocks_per_group;
  ext2_groupdesc_t *group_descriptor = &group_desc[block_group];
  uint32_t bitmap_block_index = group_descriptor->gd_b_bitmap;

  blk_t *bitmap_blk = blk_get(0, bitmap_block_index);

  /* Retrieve byte in which used bit of block resides */
  uint32_t byte_index = block_index / 8;
  char bitmap_byte = *(char *)(bitmap_blk->b_data + byte_index);

  blk_put(bitmap_blk);

  /* Check if the bit is set */
  char block_mask = 1 << block_index % 8;
  used = (bitmap_byte & block_mask) != 0;
  return used;
}

/* Reads i-node bitmap entry for `ino`. Returns 0 if the i-node is free,
 * 1 if it's in use, and EINVAL if `ino` value is out of range. */
int ext2_inode_used(uint32_t ino) {
  if (!ino || ino >= inode_count)
    return EINVAL;
  int used = 0;
  /* Retrieve inode bitmap from inode's group descriptor */
  uint32_t block_group = (ino - 1) / inodes_per_group;
  uint32_t inode_index = (ino - 1) % inodes_per_group;
  ext2_groupdesc_t *group_descriptor = &group_desc[block_group];
  uint32_t bitmap_block_index = group_descriptor->gd_i_bitmap;

  blk_t *bitmap_blk = blk_get(0, bitmap_block_index);

  /* Retrieve byte in which used bit of inode resides */
  uint32_t byte_index = inode_index / 8;
  char bitmap_byte = *(char *)(bitmap_blk->b_data + byte_index);

  blk_put(bitmap_blk);

  /* Check if the bit is set */
  char inode_mask = 1 << inode_index % 8;
  used = (bitmap_byte & inode_mask) != 0;
  return used;
}

/* Reads i-node identified by number `ino`.
 * Returns 0 on success. If i-node is not allocated returns ENOENT. */
static int ext2_inode_read(off_t ino, ext2_inode_t *inode) {
  /* Check if the inode is used */
  int used = ext2_inode_used(ino);
  if (used == false || used == EINVAL)
    return ENOENT;

  /* Retrieve block in which the inode is stored */
  uint32_t block_group = (ino - 1) / inodes_per_group;
  uint32_t inode_index = (ino - 1) % inodes_per_group;
  ext2_groupdesc_t *group_descriptor = &group_desc[block_group];
  uint32_t table_block_index = group_descriptor->gd_i_tables;
  uint32_t containing_block = inode_index / BLK_INODES;
  inode_index %= BLK_INODES;

  blk_t *inode_table_block = blk_get(0, table_block_index + containing_block);

  /* Copy the node into provided buffer */
  ext2_inode_t *src_inode =
    (ext2_inode_t *)inode_table_block->b_data + inode_index;
  memcpy(inode, src_inode, sizeof(ext2_inode_t));

  blk_put(inode_table_block);
  return 0;
}

/* Returns block pointer `blkidx` from block of `blkaddr` address. */
static uint32_t ext2_blkptr_read(uint32_t blkaddr, uint32_t blkidx) {
  blk_t *block_of_pointers = blk_get(0, blkaddr);
  uint32_t *pointers = (uint32_t *)block_of_pointers->b_data;
  uint32_t result = pointers[blkidx];
  blk_put(block_of_pointers);
  return result;
  return 0;
}

/* Translates i-node number `ino` and block index `idx` to block address.
 * Returns -1 on failure, otherwise block address. */
long ext2_blkaddr_read(uint32_t ino, uint32_t blkidx) {
  /* No translation for filesystem metadata blocks. */
  if (ino == 0)
    return blkidx;

  ext2_inode_t inode;
  if (ext2_inode_read(ino, &inode))
    return -1;

    /* Read direct pointers or pointers from indirect blocks. */
  /* Check if the index is out of bounds */
  if (blkidx >= L3_UPPER_BOUND)
    return -1;

  /* If the index is less than DP_UPPER_BOUND,
   * then it's one of the direct pointers */
  if (blkidx < DP_UPPER_BOUND)
    return inode.i_blocks[blkidx];

  /* Else we need to traverse one of the indirect pointers tree */
  uint32_t bounds[3] = {DP_UPPER_BOUND, L1_UPPER_BOUND, L2_UPPER_BOUND};
  uint32_t l_sizes[3] = {1, L1_SIZE, L2_SIZE};

  /* Choose a tree based on their sizes and given block index */
  int tree_selection = (blkidx >= L1_UPPER_BOUND) + (blkidx >= L2_UPPER_BOUND);
  uint32_t indirect = inode.i_blocks[EXT2_NDADDR + tree_selection];
  /* relative_index is an index relative to the tree we are traversing */
  uint32_t relative_index = blkidx - bounds[tree_selection];

  /* Traverse down the tree */
  for (int i = tree_selection; i >= 0; i--) {
    uint32_t next_l_size = l_sizes[i];
    uint32_t containing_block = relative_index / next_l_size;
    indirect = ext2_blkptr_read(indirect, containing_block);
    relative_index = relative_index % next_l_size;
  }

  return indirect;
  return -1;
}

/* Reads exactly `len` bytes starting from `pos` position from any file (i.e.
 * regular, directory, etc.) identified by `ino` i-node. Returns 0 on success,
 * EINVAL if `pos` and `len` would have pointed past the last block of file.
 *
 * WARNING: This function assumes that `ino` i-node pointer is valid! */
int ext2_read(uint32_t ino, void *data, size_t pos, size_t len) {
  debug("\next2_read(%d, %lu, %lu)\n", ino, pos, len);
  if (len == 0)
    return 0;
  /* Assume: len > 0 */

  /* Calculate how many blocks need to be read */
  uint32_t starting_block_idx = pos / BLKSIZE;
  uint32_t last_block = (pos + len - 1) / BLKSIZE;
  uint32_t number_of_blocks = last_block - starting_block_idx + 1;
  /* block_offset -- staring offset in the first block */
  size_t block_offset = pos % BLKSIZE;
  /* data_offset -- counts how many bytes wes already copied */
  size_t data_offset = 0;

  debug("number of blocks: %d\n", number_of_blocks);
  debug("starting block: %d\n", starting_block_idx);

  if (ino > 0) {
    /* Check if someone tries to read past the last block of inode */
    ext2_inode_t inode;
    ext2_inode_read(ino, &inode);
    uint64_t inode_size = ((uint64_t)inode.i_size_high << 32) | inode.i_size;
    uint32_t inode_datablocks =
      inode_size / BLKSIZE + (inode_size % BLKSIZE != 0);
    if (inode_datablocks < last_block + 1)
      return EINVAL;
  }

  /* Copy all the requested data into buffer */
  for (uint32_t i = 0; i < number_of_blocks; i++) {
    uint32_t block_logical_idx = starting_block_idx + i;

    size_t burst_len = BLKSIZE - block_offset;
    if (i == number_of_blocks - 1)
      burst_len = len - data_offset;

    blk_t *block = blk_get(ino, block_logical_idx);
    if (!block) {
      panic("Failed to get block!\n");
    } else if (block == BLK_ZERO) {
      memset(data, 0, burst_len);
    } else {
      memcpy(data + data_offset, block->b_data + block_offset, burst_len);
      blk_put(block);
    }

    block_offset = 0;
    data_offset += burst_len;
  }

  return 0;
}

/* Reads a directory entry at position stored in `off_p` from `ino` i-node that
 * is assumed to be a directory file. The entry is stored in `de` and
 * `de->de_name` must be NUL-terminated. Assumes that entry offset is 0 or was
 * set by previous call to `ext2_readdir`. Returns 1 on success, 0 if there are
 * no more entries to read. */
#define de_name_offset offsetof(ext2_dirent_t, de_name)

int ext2_readdir(uint32_t ino, uint32_t *off_p, ext2_dirent_t *de) {
  debug("\next2_readdir(%d, %d, %p)", ino, *off_p, de);
  if (!de | !off_p)
    return 0;

  /* Offset will point to the beginning of the next entry */
  uint32_t offset = *off_p;
  if (ext2_read(ino, de, offset, de_name_offset))
    return 0;

  debug("inode nr: %d\n", de->de_ino);

  /* Skip all of the deleted entries */
  while (de->de_ino == 0) {
    offset += de->de_reclen;
    if (ext2_read(ino, de, offset, de_name_offset))
      return 0;
    debug("inode nr: %d\n", de->de_ino);
  }

  /* Copy name into buffer, assume that user will append the '/0' character */
  ext2_read(ino, de->de_name, offset + de_name_offset, de->de_namelen);
  de->de_name[de->de_namelen] = '\0';
  debug("inode name: %s\n", de->de_name);

  /* Set offset to the start of next entry */
  *off_p = offset + de->de_reclen;
  return 1;
}

/* Read the target of a symbolic link identified by `ino` i-node into buffer
 * `buf` of size `buflen`. Returns 0 on success, EINVAL if the file is not a
 * symlink or read failed. */
int ext2_readlink(uint32_t ino, char *buf, size_t buflen) {
  int error;

  ext2_inode_t inode;
  if ((error = ext2_inode_read(ino, &inode)))
    return error;

    /* Check if it's a symlink and read it. */
  if (!S_ISLNK(inode.i_mode))
    return EINVAL;

  size_t inode_size = ((uint64_t)inode.i_size_high << 32) | inode.i_size;

  /* Check if there's enough space in the buffer */
  if (buflen < inode_size)
    return EINVAL;

  if (inode_size <= EXT2_MAXSYMLINKLEN) {
    /* Symlink target could fit into inode structure */
    memcpy(buf, inode.i_blocks, inode_size);
  } else {
    /* Symlink was to large to fit into inode, so read it the regular way */
    if (ext2_read(ino, buf, 0, inode_size))
      return EINVAL;
  }

  return 0;
}

/* Read metadata from file identified by `ino` i-node and convert it to
 * `struct stat`. Returns 0 on success, or error if i-node could not be read. */
int ext2_stat(uint32_t ino, struct stat *st) {
  int error;

  ext2_inode_t inode;
  if ((error = ext2_inode_read(ino, &inode)))
    return error;

    /* Convert the metadata! */
  st->st_ino = ino;
  st->st_mode = inode.i_mode;
  st->st_uid = ((uint32_t)inode.i_uid_high << 16) | inode.i_uid;
  st->st_gid = ((uint32_t)inode.i_gid_high << 16) | inode.i_gid;
  st->st_size = ((uint64_t)inode.i_size_high << 32) | inode.i_size;
  st->st_blocks = ((uint64_t)inode.i_nblock_high << 32) | inode.i_nblock;
  st->st_nlink = inode.i_nlink;
  st->st_atim.tv_sec = inode.i_atime;
  st->st_ctim.tv_sec = inode.i_ctime;
  st->st_mtim.tv_sec = inode.i_mtime;

  return 0;
}

/* Reads file identified by `ino` i-node as directory and performs a lookup of
 * `name` entry. If an entry is found, its i-inode number is stored in `ino_p`
 * and its type in stored in `type_p`. On success returns 0, or EINVAL if `name`
 * is NULL or zero length, or ENOTDIR is `ino` file is not a directory, or
 * ENOENT if no entry was found. */
int ext2_lookup(uint32_t ino, const char *name, uint32_t *ino_p,
                uint8_t *type_p) {
  int error;

  if (name == NULL || !strlen(name))
    return EINVAL;

  ext2_inode_t inode;
  if ((error = ext2_inode_read(ino, &inode)))
    return error;

  if (!S_ISDIR(inode.i_mode))
    return ENOTDIR;

  /* Traverse the directory, return when an entry of the same name was found */
  ext2_dirent_t de;
  uint32_t offset = 0;
  while (ext2_readdir(ino, &offset, &de)) {
    if (strcmp(de.de_name, name) == 0) {
      debug("Lookup: found %s\n", de.de_name);
      if (ino_p)
        *ino_p = de.de_ino;
      if (type_p)
        *type_p = de.de_type;
      return 0;
    }
  }

  return ENOENT;
}

/* Initializes ext2 filesystem stored in `fspath` file.
 * Returns 0 on success, otherwise an error. */
int ext2_mount(const char *fspath) {
  int error;

  if ((error = blk_init(fspath)))
    return error;

  /* Read superblock and verify we support filesystem's features. */
  ext2_superblock_t sb;
  ext2_read(0, &sb, EXT2_SBOFF, sizeof(ext2_superblock_t));

  debug(">>> super block\n"
        "# of inodes      : %d\n"
        "# of blocks      : %d\n"
        "block size       : %ld\n"
        "blocks per group : %d\n"
        "inodes per group : %d\n"
        "inode size       : %d\n",
        sb.sb_icount, sb.sb_bcount, 1024UL << sb.sb_log_bsize, sb.sb_bpg,
        sb.sb_ipg, sb.sb_inode_size);

  if (sb.sb_magic != EXT2_MAGIC)
    panic("'%s' cannot be identified as ext2 filesystem!", fspath);

  if (sb.sb_rev != EXT2_REV1)
    panic("Only ext2 revision 1 is supported!");

  size_t blksize = 1024UL << sb.sb_log_bsize;
  if (blksize != BLKSIZE)
    panic("ext2 filesystem with block size %ld not supported!", blksize);

  if (sb.sb_inode_size != sizeof(ext2_inode_t))
    panic("The only i-node size supported is %d!", sizeof(ext2_inode_t));

    /* Load interesting data from superblock into global variables.
     * Read group descriptor table into memory. */
  inode_count = sb.sb_icount;
  block_count = sb.sb_bcount;
  inodes_per_group = sb.sb_ipg;
  blocks_per_group = sb.sb_bpg;
  first_data_block = sb.sb_first_dblock;
  group_desc_count = 1 + (block_count - 1) / blocks_per_group;

  size_t group_desc_array_size = group_desc_count * sizeof(ext2_groupdesc_t);
  group_desc = malloc(group_desc_array_size);
  ext2_read(0, group_desc, EXT2_GDOFF, group_desc_array_size);

  return 0;
}
