# Example 3

```haskell
head :: [a] -> a
head []     = undefined
head (x:xs) = x
```

## Translation of Pattern Matching

```haskell
match y
  = ⟦ match [y] [([[]],undefined)
                 ,([(x:xs)],x)] Error ⟧

  -- Rule 2 - Only constructor patterns
  = case y of
      []     ->  ⟦ match [] [([],undefined)] Error⟧
      (z:zs) ->  ⟦ match [] [([x,xs],x)] Error⟧

  -- Rule 3b and 2× Rule 1 - Only variable patterns
  = case y of
      []     -> undefined
      (z:zs) -> ⟦ match [] [([],z)] Error ⟧

  -- Rule 3b
  = case y of
      []     -> undefined
      (z:zs) -> z
```

## AST Representation

The transformed version of `head` is represented by the following AST.

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

If the constructors `Nil` and `Cons` are used instead of the build-in constructors `[]` and `(:)`,

```haskell
head :: List a -> a
head y = case y of
           Nil          -> undefined
           (Cons x xs)  -> x
```

the AST looks as follows.

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
