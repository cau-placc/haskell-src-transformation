module TestModuleAlgo where

data Bool = True
          | False

data Two a = Null
           | One a
           | Two a a

data Rule l r = l :- r

data Maybe a = Just a
             | Nothing

type QueueI a = ([a], [a])

const :: a -> b -> a
const x y = x

notP :: Bool -> Bool
notP a0 = case a0 of
  True  -> False
  False -> True

addPT :: Num a => Two a -> Two a
addPT a1 = case a1 of
  One a2    -> One a2
  Two a4 a5 -> One (a4 + a5)
  Null      -> undefined

mapP :: (a -> b) -> [a] -> [b]
mapP a8 a9 = case a9 of
  []        -> []
  a11 : a12 -> a8 a11 : mapP a8 a12

zip :: [a] -> [b] -> [(a, b)]
zip a15 a16 = case a15 of
  []        -> []
  a29 : a30 -> case a16 of
    []        -> []
    a26 : a27 -> (a17, a21) : zip a18 a22

updateRule :: Rule l r -> a -> Rule l a
updateRule a31 a32 = case a31 of
  a33 :- a34 -> (a33 :- a32)

flipQ :: QueueI a -> QueueI a
flipQ a37 = case a37 of
  (a38, a39) -> case a38 of
    []        -> (reverse a39, [])
    a42 : a43 -> (a42 : a43, a39)

nullN :: [a] -> Bool
nullN a44 = case a44 of
  []        -> True
  a45 : a46 -> False

fromJustJust :: Maybe (Maybe a) -> a
fromJustJust a47 = case a47 of
  Just a48 -> case a48 of
    Just a50 -> a50
    Nothing  -> undefined
  Nothing -> undefined
