module TestModuleGE where

data Bool = True
          | False

id :: a -> a
id x = x

notT :: Bool -> Bool
notT a0
  = let a2 = undefined
        a1
          = case a0 of
                a22 -> if a22 then False else if otherwise then True else a2
      in a1

notTR :: Bool -> Bool
notTR a3
  = let a6 = undefined
        a5
          = case a3 of
                a24 -> case a24 of
                           False -> True
                           True -> a6
        a4
          = case a3 of
                a26 -> if a26 then False else a5
      in a4

notC :: Bool -> Bool
notC a7
  = let a9 = undefined
        a8
          = case a7 of
                a28 -> case a28 of
                           a29 -> let a12 = undefined
                                      a11
                                        = case a10 of
                                              a30 -> if a30 then False else
                                                       if otherwise then True else a12
                                    in a11
      in a8

notCR :: Bool -> Bool
notCR a14
  = let a16 = undefined
        a15
          = case a14 of
                a33 -> case a33 of
                           a34 -> let a20 = undefined
                                      a19
                                        = case a17 of
                                              a35 -> case a35 of
                                                         False -> True
                                                         True -> a20
                                      a18
                                        = case a17 of
                                              a37 -> if a37 then False else a19
                                    in a18
      in a15
