# Example 2

```haskell
map :: (a -> b) -> [a] -> [b]
map _ []     = []
map f (x:xs) = f x : map f xs
```

## Translation of Pattern Matching

```haskell
match g y
  = ⟦ match [g,y] [([_,[]],[])
                   ,([f,(x:xs)],f x : map f xs)] Error ⟧

  -- Rule 1 - Only variable patterns -}
  = ⟦ match [y] [([[]],[])
                 ,([(x:xs)],g x : map g xs)] Error ⟧

  -- Rule 2 - Only constructor patterns
  = case y of
      []     -> ⟦ match [] [([],[])] Error ⟧
      (z:zs) -> ⟦ match [z,zs] [([x,xs],g x : map g xs)] Error ⟧

  -- Rule 3b and Rule 1 - Only variable patterns
  = case y of
      []     -> []
      (z:zs) -> ⟦ match [zs] [([xs],g z : map g xs)] Error ⟧

  -- Rule 1 - Only variables patterns
  = case y of
      []     -> []
      (z:zs) -> ⟦ match [] [([],g z : map g zs)] Error ⟧

  -- Rule 3b
  = case y of
      []     -> []
      (z:zs) -> g z : map g zs
```

## AST Representation

The AST of the original definition of `map` with pattern-matching on the left-hand side looks as follows.

```haskell
TypeSig () [Ident () "map"]
  (TyFun ()               -- Function type `(a -> b) -> ([a] -> [b])`
    (TyParen ()           -- Parenthesis around `a -> b`
      (TyFun ()
        (TyVar ()         -- Type variable `a`
          (Ident () "a")
        )
        (TyVar ()         -- Type variable `b`
           (Ident () "b")
        )
      )
    )
    (TyFun ()             -- Function type `[a] -> [b]`
      (TyList ()          -- List type `[a]`
        (TyVar ()
          (Ident () "a")
        )
      )
      (TyList ()          -- List type `[b]`
        (TyVar ()
          (Ident () "b")
        )
      )
    )
  )
FunBind ()                   -- Function declaration for `map`
[Match ()                    -- First rule for `map`
  (Ident () "map")
  [PWildCard ()              -- Wildcard pattern `_`
  ,PList () []               -- Empty list pattern `[]`
  ]
  (UnGuardedRhs ()           -- Right-hand side of first rule
    (List () [])             -- Empty list literal `[]`
  )
  Nothing
, Match ()                   -- Second rule for `map`
  (Ident () "map")
  [PVar () (Ident () "f")    -- Variable pattern for `f`
  ,PParen ()                 -- Parenthesis around `x : xs`
  (PInfixApp ()              -- Infix constructor pattern `x : xs`
    (PVar ()                 -- Variable pattern `x`
      (Ident () "x")
    )
    (Special ()              -- The `(:)` is a special build-in constructor
      (Cons ())              -- The list constructor `(:)`
    )
    (PVar ()                 -- Variable pattern `xs`
      (Ident () "xs")
    )
  )
  ]
  (UnGuardedRhs ()           -- Right-hand side of second rule
    (InfixApp ()             -- Infix application of list constructor `(:)`
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
      (QConOp ()
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

The AST of the transformed version of `map` where the constructors `Nil` and `Cons` are used instead of `[]` and `(:)`, respectively,

```haskell
map :: (a -> b) -> List a -> List b
map f xs = case xs of
             Nil         -> Nil
             (Cons y ys) -> Cons (f y) (map f ys)

```

looks as follows.

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
