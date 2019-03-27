module Queue where

{-

Implement a persistent queue using two stacks, as described in slides
13-20 of
https://www-users.cs.umn.edu/~kauffman/2041/12-persistent-data-structs.pdf

Test it with `cabal new-test`
-}

data Queue a =
  Queue
  { incoming :: [a]
  , outgoing :: [a]
  } deriving (Show)

empty :: Queue a
empty = -- undefined -- TODO
  Queue [] []

enqueue :: Queue a -> a -> Queue a
enqueue q e = -- undefined -- TODO
  Queue (e:incoming q) (outgoing q)

dequeue :: Queue a -> Maybe (a, Queue a)
-- dequeue q = undefined -- TODO
dequeue (Queue [] []) = Nothing
dequeue (Queue i (o:os)) = Just (o, Queue i os)
dequeue (Queue i []) =
  let (o:os) = reverse i in
    Just (o, Queue [] os)

reverse_queue :: Queue a -> Queue a
-- reverse_queue q = undefined -- TODO
reverse_queue (Queue i o) = Queue o i
