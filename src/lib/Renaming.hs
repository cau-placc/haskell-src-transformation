-- | This module contains definitions for substitutions of variables.

module Renaming where

import           FreshVars
import           Language.Haskell.Exts.Syntax

-- | A substitution (or "renaming") is a mapping of variable names to variable
--   names.
type Subst = String -> String

-- Smart constructor for 'Subst' that creates a substitution that renames
-- variables with the first name to variables with the second name.
subst :: String -> String -> Subst
subst s1 s2 = \s -> if s == s1 then s2 else s

-- | Combines two substitutions.
--
--   Applying the substitution returned by @compose s1 s2@ is the same as
--   applying first @s2@ and then @s1@.
compose :: Subst -> Subst -> Subst
compose = (.)

-- | Type for substations extended from variable names to expressions.
type TSubst l
  =  Exp l -- VarExp
  -> Exp l -- Constructor application expression

-- Smart constructor for 'TSubst' that creates a substitution that replaces
-- the given variable expression (first argument) with the second expression.
tSubst :: Eq l => Exp l -> Exp l -> TSubst l
tSubst vE tE = \v -> if v == vE then tE else v

-- | Type class for AST nodes of type @c@ a 'TSubst' can be applied to.
class TermSubst c where
  -- | Applies the given substitution to the given node.
  substitute :: TSubst l -> c l -> c l

-- | 'TermSubst' instance for expressions.
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

-- | 'TermSubst' instance for @case@ expression alternatives.
--
--   Applies the substitution to the right-hand side.
instance TermSubst Alt where
  substitute s (Alt l p rhs mb) = Alt l p (substitute s rhs) mb

-- | 'TermSubst' instance for right-hand sides.
--
--   Applies the substitution to the expression on the right-hand side.
--   There must be no guards.
instance TermSubst Rhs where
  substitute s rhs = case rhs of
    UnGuardedRhs l e -> UnGuardedRhs l $ substitute s e
    GuardedRhss  _ _ -> error "TermSubst: GuardedRhss not supported"

-- | Type class for AST nodes of type @c@ a 'Subst' can be applied to.
class Rename c where
  -- | Renames all variables that occur in the given AST node.
  rename :: Subst -> c l -> c l

-- | 'Rename' instance for expressions.
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

-- | 'Rename' instance for optionally qualified variable names.
instance Rename QName where
  rename s qname = case qname of
    -- TODO qualified variables should not be renamed.
    Qual l mname name -> Qual l mname (rename s name)
    UnQual  l name    -> UnQual l (rename s name)
    Special l special -> Special l special

-- | 'Rename' instance for variable names.
instance Rename Name where
  rename s name = case name of
    Ident  l str -> Ident l $ s str
    Symbol l str -> Ident l $ s str

-- | 'Rename' instance for @case@ expression alternatives.
--
--   Variables are renamed on the right-hand side and in variable patterns.
--   TODO is it actually wanted to replace variable binders?
instance Rename Alt where
  rename s (Alt l p rhs mB) = Alt l (rename s p) (rename s rhs) mB

-- | 'Rename' instance for patterns.
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

-- | 'Rename' instance for right-hand sides.
--
--   Applies the substitution to the expression on the right-hand side.
--   There must be no guards.
instance Rename Rhs where
  rename s rhs = case rhs of
    UnGuardedRhs l e -> UnGuardedRhs l $ rename s e
    GuardedRhss  _ _ -> error "Rename: GuardedRhss not supported"

-- | Creates a new fresh variable pattern for the given variable pattern.
renamePVar :: Pat l -> PM (Pat l)
renamePVar (PVar l name) = do
  nname <- newName name
  return (PVar l nname)
renamePVar _ = error "no variable in renamePVar"

-- | Generates a fresh variable name with an ID from the state.
--
--   The given argument must be an identifier. Only the annotation of the
--   identifier is preserved.
newName :: Name l -> PM (Name l)
newName (Ident l _) = do
  var <- freshVar
  return (Ident l ('a' : show var))
newName _ = error "no Ident in newName"

-- | Generates a fresh variable name with an ID from the state.
genVar :: PM (Name ())
genVar = do
  x <- freshVar
  return (Ident () ('a' : show x))
