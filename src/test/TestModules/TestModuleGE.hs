module TestModuleGE where

data Bool = True | False

-- function containing no Guards
id :: a -> a
id x = x

-- guard on top-level without rule order
notT :: Bool -> Bool
notT b | b         = False
       | otherwise = True

-- guard on top-level with rule oder
notTR :: Bool -> Bool
notTR b | b = False
notTR False = True

-- guard on case-level without order
notC :: Bool -> Bool
notC b = case b of
           x | x         -> False
             | otherwise -> True

-- guard on case-level with rule order
notCR :: Bool -> Bool
notCR b = case b of
            x | x -> False
            False -> True
