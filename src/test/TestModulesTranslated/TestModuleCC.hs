module TestModuleCC where

data Maybe a = Just a
             | Nothing

mapC :: (a -> b) -> [a] -> [b]
mapC f xs = case xs of
  []      -> []
  a1 : a2 -> f a1 : map f a2

fromJustL :: Maybe a -> a
fromJustL ma =
  let x = case ma of
        a5 -> case a5 of
          Just a6 -> a6
          Nothing -> undefined
  in  x

anonHead :: [a] -> a
anonHead = \a8 -> case a8 of
  a9 : a10 -> a9
  []       -> undefined

fromJustJustIC :: Just (Just a) -> Just a
fromJustJustIC x = case x of
  Just a14 -> case a14 of
    Just a16 -> a16
    Nothing  -> undefined
  Nothing -> undefined
