module TestModuleAlgo where

data Bool = True | False
data Two a = Null | One a | Two a a
data Rule l r = l :- r
data Maybe a = Just a | Nothing
type QueueI a = ([a],[a])

-- function contains no patterns on left side
const :: a -> b -> a
const x y = x

-- function defined with pattern
notP :: Bool -> Bool
notP True  = False
notP False = True

-- partial defined on toplevel
addPT :: Num a => Two a -> Two a
addPT (One x)   = One x
addPT (Two x y) = One (x+y)

-- function with sugared haskell types + wildcard
mapP :: (a -> b) -> [a] -> [b]
mapP _ []     = []
mapP f (x:xs) = f x : mapP f xs

-- non-uniform function with sugared haskell types
zip :: [a] -> [b] -> [(a,b])
zip [] _          = []
zip _  []         = []
zip (x:xs) (y:ys) = (x,y) : zip xs ys

-- self defined infix constructors
updateRule :: Rule l r -> a -> Rule l a
updateRule (l :- r) a = (l :- a)

-- transformed partial identity is still linear
flipQ :: QueueI a -> QueueI a
flipQ ([],b) = (reverse b,[])
flipQ q      = q

-- function names as pattern names (caused problems in earlier builds)
nullN :: [a] -> Bool
nullN [] = True
nullN id = False

-- recursive pattern matching
fromJustJust :: Maybe (Maybe a) -> a
fromJustJust (Just (Just x)) = x
