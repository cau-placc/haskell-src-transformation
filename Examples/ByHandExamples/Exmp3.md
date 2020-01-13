# Example 3)

```haskell
head :: [a] -> a
head []     = undefined
head (x:xs) = x
```

##### 1. Übersetzung des Pattern Matchings:

----------

```haskell
match y 
  = [| match [y] [([[]],undefined)
                 ,([(x:xs)],x)] Error |]
  
  {- 2. Regel - Nur Konstruktoren  -}
  = case y of
      []     ->  [| match [] [([],undefined)] Error|]
      (z:zs) ->  [| match [] [([x,xs],x)] Error|]
  
  {- Regel 3b.-}{- 2x 1. Regel - Nur Variablen  -}
  = case y of 
      []     -> undefined
      (z:zs) -> [| match [] [([],z)] Error |]

  {- Regel 3b.-}
  = case y of 
      []     -> undefined
      (z:zs) -> z
```

----

Diese Schreibweise von `head` führt zu folgendem AST:

```haskell
TypeSig () 
  [Ident () "head"] 
  (TyFun () 
    (TyList () 
      (TyVar () 
        (Ident () "a")
      )
    )
    (TyVar () 
      (Ident () "a")
    )
  )
FunBind () 
  [Match () 
  (Ident () "head") 
  [PList () []] 
  (UnGuardedRhs () 
    (Var () 
      (UnQual ()
        (Ident () "undefined")
      )
    )
  ) 
  Nothing
  ,Match ()
  (Ident () "head") 
  [PParen ()           -- Pattern with parenthesized pattern
    (PInfixApp ()      -- Pattern with infix data constructor	
      (PVar () 
        (Ident () "x")
      ) 
      (Special ()      -- built-in constructor with special syntax
        (Cons ())
      ) 
      (PVar () 
        (Ident () "xs")
      )
    )
  ] 
  (UnGuardedRhs ()     -- expression without guard
    (Var () 
      (UnQual ()       -- unqualified local name
        (Ident () "x")
       )
     )
   )
   Nothing
   ]
```

----

Die Funktion mit aufgelöstem Pattern matching aus 1.  mit der Ergänzung, das der Listentyp ausgeschrieben wird.

```haskell
head :: List a -> a 
head y = case y of
           Nil          -> undefined
           (Cons x xs)  -> x
```

Dann sieht der AST wie folgt aus:

```haskell
TypeSig () 
  [Ident () "head"] 
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
    (TyVar () 
      (Ident () "a")
    )
  )
FunBind () 
  [Match ()	 
    (Ident () "head") 
    [PVar () 
      (Ident () "y")
    ] 
    (UnGuardedRhs () 
      (Case () 
        (Var () 
          (UnQual () 
            (Ident () "y")
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
            (Var () 
              (UnQual () 
                (Ident () "undefined")
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
                (Ident () "x")
              ,PVar () 
                (Ident () "xs")
              ]
            )
          ) 
        (UnGuardedRhs () 
          (Var () 
            (UnQual () 
              (Ident () "x")
            )
          )
        ) 
        Nothing
        ]
      )
    ) 
    Nothing
  ]
```

