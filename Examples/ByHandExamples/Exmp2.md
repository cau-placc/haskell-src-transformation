# Example 2)

```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```

  ##### 1. Übersetzung des Pattern Matches

-----

```haskell
match g y 
  = [| match [g,y] [([_,[]],[])
                   ,([f,(x:xs)],f x : map f xs)] Error |]
  
  {- 1. Regel - Nur Variablen -}
  = [| match [y] [([[]],[])
                 ,([(x:xs)],g x : map g xs)] Error |]
  
  {- 2. Regel - Nur Patterns -}
  = case y of
      []     -> [| match [] [([],[])] Error |]
      (z:zs) -> [| match [z,zs] [([x,xs],g x : map g xs)] Error |]
      
  {- Regel 3b.-}{- 1. Regel - Nur Variablen-}    
  = case y of 
      []     -> []
      (z:zs) -> [| match [zs] [([xs],g z : map g xs)] Error |]
  
  {- 1. Regel - Nur Variablen -}
  = case y of 
      []     -> []
      (z:zs) -> [| match [] [([],g z : map g zs)] Error |]
  
  {- Regel 3b. -}
  = case y of 
      []     -> []
      (z:zs) -> g z : map g zs  
```

----

**2. AST zur Funktion in mehreren Versionen**

```haskell
	map :: (a -> b) -> [a] -> [b]
    map _ []     = []          
    map f (x:xs) = f x : (map f xs)
```

Diese Schreibweise von `map` führt zu folgendem AST:

```haskell
TypeSig () [Ident () "map"] 
  (TyFun ()               -- der mittlere Funktionspfeil
    (TyParen ()           -- Klammerung in einem Typ
      (TyFun ()             
        (TyVar ()         -- Typvariable 
          (Ident () "a")  -- Identifikator a
        )
        (TyVar ()       
           (Ident () "b")
        )
      )
    ) 
    (TyFun () 
      (TyList ()          -- Listentyp 
        (TyVar () 
          (Ident () "a")
        )
      )
      (TyList () 
        (TyVar () 
          (Ident () "b")
        )
      )
    )
  )                       
FunBind ()                   -- Funktionskörper, Liste aller Regeln
[Match ()                    -- Beginn eines Patternmatchings
  (Ident () "map") 
  [PWildCard ()              -- Wildcard
  ,PList () []               -- Leere Liste  
  ] 
  (UnGuardedRhs ()           -- Beginn der Rechten Regelseite
    (List () [])
  ) 
  Nothing
  ,Match () 
  (Ident () "map") 
  [PVar () (Ident () "f")
  ,PParen () 
  (PInfixApp ()              -- infix Konstruktor 
    (PVar () 
      (Ident () "x")
    ) 
    (Special ()              -- Kennzeichnung 
      (Cons ())              -- Listenkonstruktor 
    ) 
    (PVar () 
      (Ident () "xs")
    )
  )
  ] 
  (UnGuardedRhs () 
    (InfixApp ()             -- Infix anstatt PInfix
      (App () 
        (Var ()
          (UnQual ()
            (Ident () "f")
          )
        ) 
        (Var () 
          (UnQual () 
            (Ident () "x")
          )
        )
      ) 
      (QConOp ()             -- 
        (Special () 
          (Cons ())
        )
      ) 
      (Paren () 
        (App () 
          (App () 
            (Var () 
              (UnQual () 
                (Ident () "map")
              )
            ) 
            (Var () 
              (UnQual () 
                (Ident () "f")
              )
            )
          ) 
          (Var () 
            (UnQual () 
              (Ident () "xs")
            )
          )
        )
      )
    )
  ) 
  Nothing
  ]
```

Die Funktion mit aufgelöstem Pattern matching aus 1.  mit der Ergänzung, das der Listentyp ausgeschrieben wird.

```haskell
map :: (a -> b) -> List a -> List b
map f xs = case xs of
             Nil         -> Nil
             (Cons y ys) -> Cons (f y) (map f ys)

```

Dann sieht der AST wie folgt aus: 

```haskell 
TypeSig () 
  [Ident () "map"] 
  (TyFun () 
    (TyParen () 
      (TyFun () 
        (TyVar () 
          (Ident () "a")
        )
        (TyVar () 
          (Ident () "b")
        )
      )
    ) 
    (TyFun () 
      (TyApp () 
        (TyCon () 
          (UnQual () 
            (Ident () "List")
          )
        ) 
        (TyVar () 
          (Ident () "a")
        )
      ) 
      (TyApp () 
        (TyCon () 
          (UnQual () 
            (Ident () "List")
          )
        ) 
        (TyVar () 
          (Ident () "b")
        )
      )
    )
  )
  
FunBind () 
  [Match () 
    (Ident () "map") 
    [PVar () 
      (Ident () "f")
    ,PVar () 
      (Ident () "xs")
    ] 
    (UnGuardedRhs () 
      (Case () 
        (Var () 
          (UnQual () 
            (Ident () "xs")
          )
        ) 
        [Alt () 
          (PApp () 
            (UnQual () 
              (Ident () "Nil")
            ) 
            []
          ) 
          (UnGuardedRhs () 
            (Con () 
              (UnQual () 
                (Ident () "Nil")
              )
            )
          ) 
          Nothing
        ,Alt ()
          (PParen () 
            (PApp () 
              (UnQual () 
                (Ident () "Cons")
              ) 
              [PVar () 
                (Ident () "y")
              ,PVar () 
                (Ident () "ys")
              ]
            )
          ) 
          (UnGuardedRhs () 
            (App () 
              (App () 
                (Con () 
                  (UnQual () 
                    (Ident () "Cons")
                  )
                ) 
                (Paren () 
                  (App () 
                    (Var () 
                      (UnQual () 
                        (Ident () "f")
                      )
                    ) 
                    (Var () 
                      (UnQual () 
                        (Ident () "y")
                      )
                    )
                  )
                )
              ) 
              (Paren () 
                (App () 
                  (App () 
                    (Var () 
                      (UnQual () 
                        (Ident () "map")
                      )
                    ) 
                    (Var () 
                      (UnQual () 
                        (Ident () "f")
                      )
                    )
                  ) 
                  (Var () 
                    (UnQual () 
                      (Ident () "ys")
                    )
                  )
                )
              )
            )
          ) 
          Nothing
        ]
      )
    )
  ]
   
```

