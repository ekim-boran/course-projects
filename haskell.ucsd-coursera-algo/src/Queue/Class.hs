module Queue.Class where

class Queue q where
  empty :: q a
  isEmpty :: q a -> Bool
  peekFront :: q a -> Maybe a
  popFront :: q a -> Maybe (q a)

  pushBack :: a -> q a -> q a

class (Queue q) => Dequeue q where
  peekBack :: q a -> Maybe a
  popBack :: q a -> Maybe (q a)
  pushFront :: a -> q a -> q a
