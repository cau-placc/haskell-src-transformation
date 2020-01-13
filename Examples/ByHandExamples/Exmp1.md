# Example 1)  

```haskell
zip :: [a] -> [b] -> [(a,b)]
zip []     bs     = []
zip as     []     = []
zip (a:as) (b:bs) = (a,b) : zip as bs
```



##### 1. Übersetzung des Pattern Matchings:

----



```haskell
match x y 
  = [| match [x,y] [([[],bs],[]) 
                   ,([as,[]],[]) 
                   ,([(a:as),(b:bs)],(a,b) : zip as bs)] Error |]
   		  
  {- 4. Verschiedene Pattern und Variablen -}
  = [| match [x,y] [([[],bs],[])]  		  
        ( match [x,y] [([as,[]],[])]
          ( match [x,y] [([(a:as),(b:bs)],(a,b) : zip as bs)]  
              Error ))|]
             
   {- 2. Nur Konstruktoren -}
   = case x of
       []     -> [| match [y] [([bs],[])]
                      ( match [x,y] [([as,[]],[])]
                        ( match [x,y] [([(a:as),(b:bs)],(a,b) : zip as bs)]
                            Error ))|]                        
       (z:zs) -> [| match [z,zs,y] [] 
             	    ( match [x,y] [([as,[]],[])]
                      ( match [x,y] [([(a:as),(b:bs)],(a,b) : zip as bs)]  
                        Error ))|]
             
   {- 1. Nur Variablen , 3 mal 1. Regel -}
   = case x of
       []     -> [| match [] [([],[])] |]
                    ( match [x,y] [([as,[]],[])]
                      ( match [x,y] [([(a:as),(b:bs)],(a,b) : zip as bs)]  
                        Error ))|]          
       (z:zs) -> [| match [] [] 
             	    ( match [x,y] [([as,[]],[])]
                      ( match [x,y] [([(a:as),(b:bs)],(a,b) : zip as bs)]  
                        Error ))|]
                 
   {- 3b -}{- 3a -}
   = case x of
       []     ->  []          	  
       (z:zs) -> [| match [x,y] [([as,[]],[])]
                    ( match [x,y] [([(a:as),(b:bs)],(a,b) : zip as bs)]  
                      Error )|]
             
   {- 1. Nur Variablen -}
   = case x of
       []     -> []              
       (z:zs) -> [| match [y] [([[]],[])]
                    ( match [x,y] [([(a:as),(b:bs)],(a,b) : zip as bs)]  
                      Error )|]
                       
   {- 2. Nur Konstruktoren -}
   = case x of 
       []     -> []              
       (z:zs) -> case y of 
                   []     -> [| match [] [([],[])] 
                          	    ( match [x,y] [([(a:as),(b:bs)],(a,b) : zip as bs)]  
                                  Error )|]
                   (q:qs) -> [| match [q,qs] [] 
                                ( match [x,y] [([(a:as),(b:bs)],(a,b) : zip as bs)]  
                                  Error )|]
   {- 3b. -}{- 2 mal 1. Regel -}
   = case x of 
       []     -> []              
       (z:zs) -> case y of 
                   []     -> []                   
                   (q:qs) -> [| match [] [] 
                                ( match [x,y] [([(a:as),(b:bs)],(a,b) : zip as bs)]  
                                  Error )|]
   {- 3a. -}
   = case x of 
       []     -> []  
       (z:zs) -> case y of 
                   []     -> []                   
                   (q:qs) -> [| match [x,y] [([(a:as),(b:bs)],(a,b) : zip as bs)]  
                                Error |]
                                   
   {- 2. -}
   = case x of 
       []     -> []              
       (z:zs) -> case y of 
                   []     -> []                   
                   (q:qs) -> case x of
                               []     -> [| match [y] [] Error |]                               
                               (t:ts) -> [| match [t,ts,y] 
                                            [([a,as,(b:bs)],(a,b) : zip as bs)]
                                            Error |]
                                               
   {- 1. Nur Variablen-}{-2 mal 1. Regel-}
   = case x of 
       []     -> []              
       (z:zs) -> case y of 
                   []     -> []                   
                   (q:qs) -> case x of
                               []     -> [| match [] [] Error |]                         
                               (t:ts) -> [| match [y] 
                                            [([(b:bs)],(t,b) : zip ts bs)]
                                            Error |]
                                               
   {- 3a. -}{- 2. Nur Konstruktoren -}
   = case x of 
       []     -> []              
       (z:zs) -> case y of
                   []     -> []                   
                   (q:qs) -> case x of
                               []     -> error                               
                               (t:ts) -> case y of
                                           []     -> [| match [] [] Error |]
                                           (u:us) -> [| match [u,us] 
                                                        [([b,bs],(t,b) : zip ts bs)]
                                                        Error   |]
                                                                                                             
   {-3a-}{- 2 mal 1. Regel -}
   = case x of
       []     -> []                 
       (z:zs) -> case y of 
                   []     -> []                      
                   (q:qs) -> case x of
                               []     -> error                                 
                               (t:ts) -> case y of
                                           []     -> error                               
                                           (u:us) -> [| match [] 
                                                        [([],(t,u) : zip ts us)]
                                                        Error |]
      
   {-3b-}
   = case x of 
       []     -> []                 
       (z:zs) -> case y of 
                   []     -> []                      
                   (q:qs) -> case x of
                               []     -> error                               
                               (t:ts) -> case y of
                                           []     -> error
                                           (u:us) -> (t,u) : zip ts us
```

