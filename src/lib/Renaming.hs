module Renaming where

import           FreshVars
import           Language.Haskell.Exts.Syntax

-- | The type 'Subst' is a Substitution from String to String (a renaming).
type Subst = String -> String

-- |The function 'subst' is a smart constructor for the type Subst.
-- It takes two strings and returns a Subst that renames the first into the second.
subst :: String -> String -> Subst
subst s1 s2 = \s -> if s == s1 then s2 else s

-- |The composition of two substitutions is the sequential application of both
-- substitutions. Therefore it is equal to function composition (.).
compose :: Subst -> Subst -> Subst -- f (g x) = (f . g) x
compose = (.)

-- |The type 'TSubst' is a substitution from a given var expression with another expression.
type TSubst l
  =  Exp l -- VarExp
  -> Exp l -- Constructor application expression

-- |The function 'tSubst' is a smart constructor for the type TSubst.
-- It takes two expressions and returns a TSubst that substitutes the first with the second if applied.
tSubst :: Eq l => Exp l -> Exp l -> TSubst l
tSubst vE tE = \v -> if v == vE then tE else v

-- |The class TermSubst defines a function substitute to apply tSubst.
class TermSubst c where
    substitute :: TSubst l -> c l -> c l

-- |The TermSubst instance for Expressions.
-- Applying a TermSubst to an Expression is done by recursion over
-- the AST. Therefore TermSubst instances for RHS and Alt are needed.
instance TermSubst Exp where
  substitute s expr = case expr of
    Var l qname          -> s (Var l qname)
    Con l qname          -> Con l qname
    Lit l literal        -> Lit l literal
    InfixApp l e1 qop e2 -> InfixApp l (substitute s e1) qop (substitute s e2)
    App    l e1 e2       -> App l (substitute s e1) (substitute s e2)
    Lambda l ps e        -> Lambda l ps (substitute s e)
    Let    l b  e        -> Let l b $ substitute s e
    If l e1 e2 e3 -> If l (substitute s e1) (substitute s e2) (substitute s e3)
    Case  l e   as       -> Case l e (map (substitute s) as)   -- no subst for debugging (substitute s e)
    Tuple l bxd es       -> Tuple l bxd (map (substitute s) es)
    List  l es           -> List l (map (substitute s) es)
    Paren l e            -> Paren l (substitute s e)
    ListComp   _ _ _     -> error "TermSubst: List comp is not supported"
    ExpTypeSig l e t     -> ExpTypeSig l (substitute s e) t
    _                    -> error "TermSubst: Exp caused an error"

instance TermSubst Alt where
  substitute s (Alt l p rhs mb) = (Alt l p (substitute s rhs) mb)

instance TermSubst Rhs where
  substitute s rhs = case rhs of
    UnGuardedRhs l e -> UnGuardedRhs l $ substitute s e
    GuardedRhss  _ _ -> error "TermSubst: GuardedRhss not supported"

-- |The TermSubst instance for Expressions.
-- Applying a Subst to an Expression is done by recursion over
-- the AST. Therefore Rename instances for RHS, Alt, QName, Name and Pat are needed.
class Rename c where
    rename :: Subst -> c l -> c l

-- |The
instance Rename Exp where
    -- rename :: Subst -> Exp l -> Exp l
  rename s expr = case expr of
    Var l qname          -> Var l (rename s qname)
    Con l qname          -> Con l qname
    Lit l literal        -> Lit l literal
    InfixApp l e1 qop e2 -> InfixApp l (rename s e1) qop (rename s e2)
    App    l e1 e2       -> App l (rename s e1) (rename s e2)
    Lambda l ps e        -> Lambda l (map (rename s) ps) (rename s e)
    Let    l b  e        -> Let l b $ rename s e
    If l e1 e2 e3        -> If l (rename s e1) (rename s e2) (rename s e3)
    Case  l e   as       -> Case l (rename s e) (map (rename s) as)
    Tuple l bxd es       -> Tuple l bxd (map (rename s) es)
    List  l es           -> List l (map (rename s) es)
    Paren l e            -> Paren l (rename s e)
    ListComp   _ _ _     -> error "Rename: List comp is not supported"
    ExpTypeSig l e t     -> ExpTypeSig l (rename s e) t
    _                    -> error "Rename: Exp caused an error"
instance Rename QName where
  rename s qname = case qname of
    Qual l mname name -> Qual l mname (rename s name)
    UnQual  l name    -> UnQual l (rename s name)
    Special l special -> Special l special

instance Rename Name where
  rename s name = case name of
    Ident  l str -> Ident l $ s str
    Symbol l str -> Ident l $ s str

instance Rename Alt where
  rename s (Alt l p rhs mB) = Alt l (rename s p) (rename s rhs) mB

instance Rename Pat where
  rename s pat = case pat of
    PVar l name -> PVar l $ rename s name
    PInfixApp l p1 qname p2 ->
      PInfixApp l (rename s p1) (rename s qname) (rename s p2)
    PApp   l qname ps -> PApp l (rename s qname) (map (rename s) ps)
    PTuple l boxed ps -> PTuple l boxed (map (rename s) ps)
    PList  l ps       -> PList l (map (rename s) ps)
    PParen l p        -> PParen l $ rename s p
    PWildCard l       -> PWildCard l
    _                 -> error "Rename: Pat caused an error"

instance Rename Rhs where
  rename s rhs = case rhs of
    UnGuardedRhs l e -> UnGuardedRhs l $ rename s e
    GuardedRhss  _ _ -> error "Rename: GuardedRhss not supported"



renamePVar :: Pat l -> PM (Pat l)
renamePVar (PVar l name) = do
  nname <- newName name
  return (PVar l nname)
renamePVar _ = error "no variable in renamePVar"

newName :: Name l -> PM (Name l)
newName (Ident l _) = do
  var <- freshVar
  return (Ident l ('a' : show var))
newName _ = error "no Ident in newName"

genVar :: PM (Name ())
genVar = do
  x <- freshVar
  return (Ident () ('a' : show x))
