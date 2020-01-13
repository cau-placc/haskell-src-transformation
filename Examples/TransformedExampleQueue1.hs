module ExampleQueue1 where

type Queue a = [a]

empty :: Queue a
empty = []

isEmpty :: Queue a -> Bool
isEmpty q = null q

front :: Queue a -> a
front a0
  = case a0 of
        a1 : a2 -> a1
        []      -> undefined

add :: a -> Queue a -> Queue a
add x q = q ++ [x]
