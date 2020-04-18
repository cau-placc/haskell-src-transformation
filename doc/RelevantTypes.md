# Relevant Types

This is a (growing) summary of AST types and constructors which are relevant for the transformations.

## Names

```haskell
data QName l
  = Qual l (ModuleName l) (Name l) -- represents qualified vars and constructor
  | UnQual l (Name l)              -- unqualified local name
  | Special l (SpecialCon l)       -- haskells build-in sugar syntax

data QOp l                         -- qualified infix operators
  = QVarOp l (QName l)             -- variable operator
  | QConOp l (QName l)             -- constructor operator
```

## Modules and Declarations

```haskell
data Module l
  = Module l (Maybe(ModuleHead l))
             [ModulePragma l]
             [ImportDecl l]
             [Decl l]

data Decl l                       -- declarations
  | TypeSig l [Name l] (Type l)   -- type signature
  | FunBind l [Match l]           -- set of function binding clauses ,

data Match l
  = Match l (Name l) [Pat l] (Rhs l) (Maybe (Binds l))
    -- function name with argument patterns
  | InfixMatch l (Pat l) (Name l) [Pat l] (Rhs l) (Maybe (Binds l))
    -- clause defined with infix notation
```

## Right-Hand Sides

```haskell
data Rhs l
  = UnGuardedRhs l (Exp l)         -- pure expression
  | GuardedRhss l [GuardedRhs l]   -- guarded right hand side

data GuardedRhs l [Stmt l] (Exp l) -- | stmts = exp

data Stmt l
  = Generator l (Pat l) (Exp l)    -- pat <- exp
  | Qualifier l (Exp l)            -- an expression
  | LetStmt (Binds l)              -- a local binding
  | RectStmt                       -- recursive binding (unused)
```

## Expressions

```haskell
data Exp l
  = Var l (QName l)                    -- variable
  | Con l (QName l)                    -- constructor
  | Lit l (Literal l )                 -- literal constant
  | InfixApp l (Exp l) (QOp l) (Exp l) -- infix application
  | App l (Exp l) (Exp l)              -- application
  | NegApp l (Exp l)                   -- negation of an expression "-"
  | Lambda l [Pat l] (Exp l)           -- lambda expression
  | Let l (Binds l) (Exp l)            -- local declaration
  | If l (Exp l) (Exp l) (Exp l)       -- if e then e_1 else e_2
  | Case l (Exp l) [Alt l]             -- case exp of alts
  | Do l [Stmt l]                      -- do expression
  | Tuple l Boxed [Exp l]              -- tuple expression
  | List l [Exp l]                     -- list expression
  | Paren l (Exp l)                    -- parenthesised expression
  | ListComp l (Exp l )                -- list comprehension
  | EnumFrom l (Exp l)                       -- [from ..]
  | EnumFromTo l (Exp l) (Exp l)             -- [from .. to]
  | EnumFromThenTo l (Exp l) (Exp l) (Exp l) -- [from,then .. to]
```

## Patterns

```haskell
data Pat l                      -- differen patterns
  = Plist l [Pat l]             -- list Pattern (can contain other patterns)
  | PParen l (Pat l)            -- a pattern in parenthesis
  | PAsPat l (Name l) (Pat l)   -- as pattern
  | PVar (Name l)               -- variable Pattern
  | PLit (Sign l) (Literal l)   -- a literal constant
  | PWildCard l                 -- a wildcard Pattern
  | PApp (QName l) [Pat l]      -- data constructor pattern (list of arguments)
```
