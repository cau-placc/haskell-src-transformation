module TestModuleCC where

data Maybe a = Just a | Nothing

-- case completion on complete functions does nothing
mapC :: (a -> b) -> [a] -> [b]
mapC f xs = case xs of
              []     -> []
              (y:ys) -> f y : map f ys

-- case completion for right side of let-expression
fromJustL :: Maybe a -> a
fromJustL ma = let x = case ma of
                          Just a -> a
               in x

-- Support for anonymus functions
anonHead :: [a] -> a
anonHead = \ (x:xs) -> x

-- nested Pattern incomplete
fromJustJustIC :: Just (Just a) -> Just a
fromJustJustIC x = case x of
                     Just (Just a) -> a
