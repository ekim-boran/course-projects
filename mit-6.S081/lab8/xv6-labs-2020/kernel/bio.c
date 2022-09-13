// Buffer cache.
//
// The buffer cache is a linked list of buf structures holding
// cached copies of disk block contents.  Caching disk blocks
// in memory reduces the number of disk reads and also provides
// a synchronization point for disk blocks used by multiple processes.
//
// Interface:
// * To get a buffer for a particular disk block, call bread.
// * After changing buffer data, call bwrite to write it to disk.
// * When done with the buffer, call brelse.
// * Do not use the buffer after calling brelse.
// * Only one process at a time can use a buffer,
//     so do not keep them longer than necessary.

#include "types.h"
#include "param.h"
#include "spinlock.h"
#include "sleeplock.h"
#include "riscv.h"
#include "defs.h"
#include "fs.h"
#include "buf.h"

#define NBUCKET 13
struct spinlock lock;
struct buf bufs[NBUF];

struct bucket
{
  struct spinlock lock;
  struct buf *buf;
};

struct bucket table[NBUCKET];
void remove(struct buf *b, struct bucket *bucket)
{
  struct buf **t = &bucket->buf;

  for (; *t != 0; t = &(*t)->next)
  {
    if (*t == b)
    {
      *t = b->next;
      b->next = 0;
      break;
    }
  }
}

void insert(struct buf *b, struct bucket *bucket)
{
  b->next = bucket->buf;
  bucket->buf = b;
}
void binit(void)
{
  initlock(&lock, "bcache");
  for (int i = 0; i < NBUCKET; i++)
  {
    initlock(&table->lock, "bcache.bucket");
    table->buf = 0;
  }
  for (int b = 0; b < NBUF; b++)
  {
    memset(&bufs[b], 0, sizeof(struct buf));
    initsleeplock(&bufs[b].lock, "buffer");
    insert(&bufs[b], &table[0]);
  }
}

void acquire_ordered(int x, int y)
{
  struct bucket *x_bucket = &table[x];
  struct bucket *y_bucket = &table[y];

  if (x < y)
  {
    acquire(&x_bucket->lock);
    acquire(&y_bucket->lock);
  }
  else if (x == y)
  {
    acquire(&x_bucket->lock);
  }
  else
  {
    acquire(&y_bucket->lock);
    acquire(&x_bucket->lock);
  }
}
void release_ordered(int x, int y)
{
  struct bucket *x_bucket = &table[x];
  struct bucket *y_bucket = &table[y];

  if (x < y)
  {
    release(&x_bucket->lock);
    release(&y_bucket->lock);
  }
  else if (x == y)
  {
    release(&x_bucket->lock);
  }
  else
  {
    release(&y_bucket->lock);
    release(&x_bucket->lock);
  }
}


static struct buf *
bget(uint dev, uint blockno)
{
  struct buf *b;
  int target_index = blockno % NBUCKET;
  struct bucket *bucket = &table[target_index];
  acquire(&bucket->lock);
  for (b = bucket->buf; b != 0; b = b->next)
  {
    if (b->dev == dev && b->blockno == blockno)
    {
      b->refcnt++;
      release(&bucket->lock);
      acquiresleep(&b->lock);
      return b;
    }
  }
  release(&bucket->lock);

  acquire(&lock);

  acquire(&bucket->lock);
  for (b = bucket->buf; b != 0; b = b->next)
  {
    if (b->dev == dev && b->blockno == blockno)
    {
      b->refcnt++;
      release(&lock);
      release(&bucket->lock);
      acquiresleep(&b->lock);
      return b;
    }
  }
  release(&bucket->lock);
  while (1)
  {
    int mincount = __UINT32_MAX__;
    struct buf *result = 0;
    for (int i = 0; i < NBUF; i++)
    {
      if (bufs[i].refcnt == 0 && bufs[i].ticks <= mincount)
      {
        result = &bufs[i];
        mincount = bufs[i].ticks;
      }
    }
    if (result == 0)
    {
      panic("no buffers");
    }
    int source_index = result->blockno % NBUCKET;
    struct bucket *rem_bucket = &table[source_index];

    acquire_ordered(source_index, target_index);
    if (result->refcnt == 0 && result->ticks == mincount)
    {
      remove(result, rem_bucket);
      result->dev = dev;
      result->blockno = blockno;
      result->valid = 0;
      result->refcnt = 1;
      insert(result, bucket);
      release_ordered(source_index, target_index);
      release(&lock);
      acquiresleep(&result->lock);
      return result;
    }
    else
    {
      release_ordered(source_index, target_index);
    }
  }

  panic("bget: no buffers");
}

struct buf *
bread(uint dev, uint blockno)
{
  struct buf *b;
  b = bget(dev, blockno);
  if (!b->valid)
  {
    virtio_disk_rw(b, 0);
    b->valid = 1;
  }
  return b;
}

void bwrite(struct buf *b)
{
  if (!holdingsleep(&b->lock))
    panic("bwrite");
  virtio_disk_rw(b, 1);
}

void brelse(struct buf *b)
{

  if (!holdingsleep(&b->lock))
    panic("brelse");
  releasesleep(&b->lock);
  int i = b->blockno % NBUCKET;
  struct bucket *bucket = &table[i];
  acquire(&bucket->lock);
  b->refcnt--;
  b->ticks = ticks;
  release(&bucket->lock);
}

void bpin(struct buf *b)
{
  int i = b->blockno % NBUCKET;
  acquire(&table[i].lock);
  b->refcnt++;
  release(&table[i].lock);
}

void bunpin(struct buf *b)
{
  int i = b->blockno % NBUCKET;
  acquire(&table[i].lock);
  b->refcnt--;
  release(&table[i].lock);
}
