#include <stdio.h>
#include <stdlib.h>
#include "types.h"
#include "gc.h"

#define OFF 1
#define VInit 0x00000000
#define VMark 0x00000001
#define VFwd(A) (A | VMark)
#define HEAPPRINT 30

////////////////////////////////////////////////////////////////////////////////
// Imported from types.c
////////////////////////////////////////////////////////////////////////////////

extern RValue is_number(RValue);
extern RValue is_tuple(RValue);
extern RValue tuple_at(RAddr base, RIndex i);
extern RIndex tuple_size(RAddr base);
extern RAddr int_addr(RValue);
extern RValue addr_int(RAddr);

////////////////////////////////////////////////////////////////////////////////

typedef struct Frame_
{
  RAddr sp;
  RAddr bp;
} Frame;

typedef enum Field_
{
  GCWord,
  Size,
  Elem,
  Stack
} Field;

typedef enum Tag_
{
  VAddr,
  VStackAddr,
  VNumber,
  VBoolean,
  VGC
} Tag;

union Data
{
  RAddr addr;
  RValue value;
  RValue gcvalue;
};

typedef struct Value_
{
  Tag tag;
  union Data data;
} Value;

////////////////////////////////////////////////////////////////////////////////
// Low-level API
////////////////////////////////////////////////////////////////////////////////

RValue valueInt(Value v)
{
  if (v.tag == VAddr || v.tag == VStackAddr)
  {
    return addr_int(v.data.addr);
  }
  else if (v.tag == VNumber)
  {
    return (v.data.value << 1);
  }
  else
  { // v.tag == VBoolean
    return v.data.value;
  }
}

Value intValue(RValue v)
{
  Value res;
  if (is_tuple(v))
  {
    res.tag = VAddr;
    res.data.addr = int_addr(v);
  }
  else if (is_number(v))
  {
    res.tag = VNumber;
    res.data.value = v >> 1;
  }
  else
  { // is_boolean(v)
    res.tag = VBoolean;
    res.data.value = v;
  }
  return res;
}

Value getElem(RAddr addr, RIndex i)
{
  RValue vi = tuple_at(addr, i);
  return intValue(vi);
}

void setElem(RAddr addr, RIndex i, Value v)
{
  addr[i + 2] = valueInt(v);
}

void setStack(RAddr addr, Value v)
{
  *addr = valueInt(v);
}

Value getStack(RAddr addr)
{
  return intValue(*addr);
}

RAddr extStackAddr(Value v)
{
  if (v.tag == VStackAddr)
    return v.data.addr;
  printf("GC-PANIC: extStackAddr");
  exit(1);
}

RAddr extHeapAddr(Value v)
{
  if (v.tag == VAddr)
    return v.data.addr;
  printf("GC-PANIC: extHeapAddr");
  exit(1);
}

void setSize(RAddr addr, RIndex n)
{
  addr[0] = (n << 1);
}

int isLive(RAddr addr)
{
  return (addr[1] == VInit ? 0 : 1);
}

void setGCWord(RAddr addr, RValue gv)
{
  if (DEBUG)
    fprintf(stderr, "\nsetGCWord: addr = %p, gv = %ld\n", addr, gv);
  addr[1] = gv;
}

RAddr forwardAddr(RAddr addr)
{
  return int_addr(addr[1]);
}

Value vHeapAddr(RAddr addr)
{
  return intValue(addr_int(addr));
}

RIndex blockSize(RAddr addr)
{
  RIndex n = tuple_size(addr);
  return (n + 2);
}
////////////////////////////////////////////////////////////////////////////////

Frame caller(RAddr stack_bottom, Frame frame)
{
  Frame callerFrame;
  RAddr bptr = frame.bp;
  if (bptr == stack_bottom)
  {
    return frame;
  }
  else
  {
    callerFrame.sp = bptr + 1;
    callerFrame.bp = (RAddr)*bptr;
    return callerFrame;
  }
}

void print_stack(RAddr stack_top, RAddr first_frame, RAddr stack_bottom)
{
  Frame frame = {stack_top - OFF, first_frame};
  if (DEBUG)
    fprintf(stderr, "***** STACK: START sp=%p, bp=%p,bottom=%p *****\n", stack_top, first_frame, stack_bottom);
  do
  {
    if (DEBUG)
      fprintf(stderr, "***** FRAME: START *****\n");
    for (RAddr p = frame.sp; p < frame.bp; p++)
    {
      if (DEBUG)
        fprintf(stderr, "  %p: %p\n", p, (RAddr)*p);
    }
    if (DEBUG)
      fprintf(stderr, "***** FRAME: END *****\n");
    frame = caller(stack_bottom, frame);
  } while (frame.sp != stack_bottom);
  if (DEBUG)
    fprintf(stderr, "***** STACK: END *****\n");
}

void print_heap(RAddr heap, RIndex size)
{
  fprintf(stderr, "\n");
  for (RIndex i = 0; i < size; i += 1)
  {
    fprintf(stderr, "  %d/%p: %p (%ld)\n", i, (heap + i), (RAddr)(heap[i]), *(heap + i));
  }
}

////////////////////////////////////////////////////////////////////////////////
// TBD:mark FILL THIS IN see documentation in 'gc.h' ///////////////////////////
////////////////////////////////////////////////////////////////////////////////

RAddr mark_tuple(RAddr a)
{

  RAddr addr = int_addr(a);
  RAddr max_addr = addr;

  int size = tuple_size(addr);

  setGCWord(addr, VMark);
  //fprintf(stderr, "mark address -->%ld %ld\n", size, addr);

  for (int i = 0; i < size; i++)
  {
    RValue f = tuple_at(addr, i);
    if (is_tuple(f))
    {
      RAddr new_addr = mark_tuple(f);
      if (new_addr > max_addr)
      {
        max_addr = new_addr;
      }
    }
  }
  return addr;
}

RAddr mark(RAddr stack_top, RAddr first_frame, RAddr stack_bottom, RAddr heap_start)
{
  RAddr max_addr = heap_start;
  Frame frame = {stack_top - OFF, first_frame};
  do
  {
    for (RAddr p = frame.sp + 1; p < frame.bp; p++)
    {
      if (is_tuple(*p))
      {
        RAddr new_addr = mark_tuple(*p);
        if (new_addr > max_addr)
        {
          max_addr = new_addr;
        }
      }
    }
    frame = caller(stack_bottom, frame);
  } while (frame.sp != stack_bottom);

  if (DEBUG)
    fprintf(stderr, "-->mark: mark_frame: stack_top = %p, first_frame = %p, stack_bottom = %p\n", stack_top, first_frame, stack_bottom);
  fprintf(stderr, "xxx mark return %p\n", max_addr);

  return max_addr;
}

////////////////////////////////////////////////////////////////////////////////
// TBD:forward FILL THIS IN see documentation in 'gc.h' ////////////////////////
////////////////////////////////////////////////////////////////////////////////
RAddr forward(RAddr heap_start, RAddr max_address)
{
  RAddr lastAlive = heap_start;
  RAddr current = heap_start;
  while (current <= max_address)
  {
    int size = tuple_size(current);
    if ((current[1] & VMark) != 0) // alive
    {
      setGCWord(current, VFwd((long)lastAlive));
      lastAlive += (size + 2);
    }
    current += (size + 2);
  }
  fprintf(stderr, "forward:: %p %p\n", heap_start, lastAlive);
  return lastAlive;
}

////////////////////////////////////////////////////////////////////////////////
// TBD:redirect FILL THIS IN see documentation in 'gc.h' ///////////////////////
////////////////////////////////////////////////////////////////////////////////

void redirect_tuple(RAddr heap_start, RAddr max_address)
{

  RAddr current = heap_start;
  while (current <= max_address)
  {
    int size = tuple_size(current);
    fprintf(stderr, "XXXX%p %p %p\n", size, current, max_address);

    if ((current[1] & VMark) != 0) // alive
    {
      for (int i = 0; i < size; i++)
      {
        RValue field = current[2 + i];

        if (is_tuple(field))
        {

          RAddr tuple_addr = int_addr(field);
          RAddr faddr = tuple_addr[1];
          fprintf(stderr, "-----------------------> %p %p\n", field, faddr);

          current[2 + i] = ((long)faddr | 1); // tag again
        }
      }
    }
    current += (size + 2);
  }
}

void redirect(RAddr stack_bottom, RAddr stack_top, RAddr first_frame, RAddr heap_start, RAddr max_address)
{
  // change stack adresses
  Frame frame = {stack_top - OFF, first_frame};
  do
  {
    for (RAddr p = frame.sp + 1; p < frame.bp; p++)
    {
      if (is_tuple(*p))
      {
        RAddr tuple_addr = int_addr(*p);
        RAddr new_addr = forwardAddr(tuple_addr);
        *p = ((long)new_addr | 1); // tag again
      }
    }
    frame = caller(stack_bottom, frame);
  } while (frame.sp != stack_bottom);

  redirect_tuple(heap_start, max_address);
}

////////////////////////////////////////////////////////////////////////////////
// TBD:compact FILL THIS IN see documentation in 'gc.h' ////////////////////////
////////////////////////////////////////////////////////////////////////////////

void compact(RAddr heap_start, RAddr max_address, RAddr heap_end)
{

  RAddr current = heap_start;
  RAddr lastAddr = heap_start;

  while (current <= max_address)
  {
    int size = tuple_size(current);
    if ((current[1] & VMark) != 0) // alive
    {
      RAddr new_addr = forwardAddr(current);
      for (int i = 0; i < size + 2; i++)
      {
        new_addr[i] = current[i];
      }
      new_addr[1] = 0;
      lastAddr = new_addr + (size + 2);
    }
    current += (size + 2);
  }
  while (lastAddr < heap_end)
  {
    lastAddr[0] = 0;
    lastAddr++;
  }
}

////////////////////////////////////////////////////////////////////////////////
// Top-level GC function (you can leave it as it is!) //////////////////////////
////////////////////////////////////////////////////////////////////////////////

RAddr gc(RAddr stack_bottom, RAddr stack_top, RAddr first_frame, RAddr heap_start, RAddr heap_end)
{
  RIndex blocks = heap_end - heap_start;
  print_stack(stack_top, first_frame, stack_bottom);
  if (DEBUG)
    fprintf(stderr, "gc: mark: heap_start=%p\n", heap_start);
  if (DEBUG)
    print_heap(heap_start, blocks);

  RAddr max_address = mark(stack_top, first_frame, stack_bottom, heap_start);

  if (DEBUG)
    fprintf(stderr, "gc: mark: end max_address=%p\n", max_address);
  if (DEBUG)
    print_heap(heap_start, blocks);

  RAddr new_address = forward(heap_start, max_address);

  if (DEBUG)
    fprintf(stderr, "gc: forward: end\n");
  if (DEBUG)
    print_heap(heap_start, blocks);

  redirect(stack_bottom, stack_top, first_frame, heap_start, max_address);

  if (DEBUG)
    fprintf(stderr, "gc: redirect: end\n");
  if (DEBUG)
    print_heap(heap_start, blocks);

  compact(heap_start, max_address, heap_end);

  if (DEBUG)
    fprintf(stderr, "gc: compact: end");
  if (DEBUG)
    print_heap(heap_start, blocks);

  return new_address;
}
