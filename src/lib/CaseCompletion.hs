module CaseCompletion
  ( completeCase
  , applyCCModule
  )
where

import           Algo
import           FreshVars
import qualified Language.Haskell.Exts.Build   as B
import           Language.Haskell.Exts.Syntax

-- takes a given expression and applies the algorithm on it resulting in completed cases
completeCase :: Bool -> Exp () -> PM (Exp ())
completeCase insideLet (Case _ expr as) = do
  v <- newVar
  let eqs = map getEqFromAlt as   -- [Eqs]
  eqs' <- mapM (\(p, ex) -> completeCase insideLet ex >>= \e -> return (p, e))
               eqs
  res <- match [v] eqs' err
  if not insideLet
    then do
      let (Case _ _ resAs) = res
      return $ B.caseE expr resAs  -- test to deconstruct the first case
    else do
      let a = B.alt v res
      return $ B.caseE expr [a]
completeCase il (InfixApp _ e1 qop e2) = do
  exp1 <- completeCase il e1
  exp2 <- completeCase il e2
  return $ InfixApp () exp1 qop exp2
completeCase il (App _ e1 e2) = do
  exp1 <- completeCase il e1
  exp2 <- completeCase il e2
  return $ App () exp1 exp2
completeCase il (Lambda _ ps    e) = completeLambda ps e il
completeCase il (Let    _ binds e) = do
  e'     <- completeCase il e -- undefined -- TODO
  binds' <- completeBindRhs binds
  return $ Let () binds' e'
completeCase il (If _ e1 e2 e3) = do
  exp1 <- completeCase il e1
  exp2 <- completeCase il e2
  exp3 <- completeCase il e3
  return $ If () exp1 exp2 exp3
completeCase il (Tuple _ boxed es) = do
  exps <- mapM (completeCase il) es  -- complete case für List Expression
  return $ Tuple () boxed exps
completeCase il (Paren _ e1) = do
  exp1 <- completeCase il e1
  return $ Paren () exp1
completeCase _ v = return v
-- TODO unhandled or not implemented cons missing. Is NegApp used?



completeBindRhs :: Binds () -> PM (Binds ())
completeBindRhs (BDecls _ dcls) = do
  dcls' <- mapM (applyCCDecl True) dcls
  return $ BDecls () dcls'
completeBindRhs _ = error "completeBindRhs: ImplicitBinds not supported yet"


getEqFromAlt :: Alt () -> Eqs
getEqFromAlt (Alt _ pat (UnGuardedRhs _ expr) _) = ([pat], expr)
getEqFromAlt _ = error "guarded Rhs in getEqFromAlt"

completeLambda :: [Pat ()] -> Exp () -> Bool -> PM (Exp ())
completeLambda ps e insideLet = do
  xs <- newVars (length ps)
  e' <- completeCase insideLet e
  let eq = (ps, e')
  res <- match xs [eq] err
  return $ Lambda () xs res


applyCCModule :: Module () -> PM (Module ())
applyCCModule (Module _ mmh mps ids ds) = do
  dcls <- mapM (applyCCDecl False) ds
  return $ Module () mmh mps ids dcls
applyCCModule _ = error "applyCCModule: not on module"

applyCCDecl :: Bool -> Decl () -> PM (Decl ())
applyCCDecl insideLet (FunBind _ ms) = do
  nms <- applyCCMatches insideLet ms
  return (FunBind () nms)
applyCCDecl insideLet (PatBind _ p r _) = if isPVar p
  then do
    let e = (\(UnGuardedRhs _ x) -> x) r
    e' <- completeCase insideLet e
    return $ PatBind () p (UnGuardedRhs () e') B.noBinds
  else error "Toplevel PatBind with no Variable"
applyCCDecl _ v = return v

applyCCMatches :: Bool -> [Match ()] -> PM [Match ()]
applyCCMatches insideLet = mapM applyCCMatch
 where
  applyCCMatch :: Match () -> PM (Match ()) -- TODO maybe only apply if needed -> isIncomplete?
  applyCCMatch (Match _ n ps rhs _) = case rhs of
    UnGuardedRhs _ e -> do
      x <- completeCase insideLet e
      return $ Match () n ps (UnGuardedRhs () x) B.noBinds
    GuardedRhss _ _ -> error "applyCCMatch: GuardedRhs found"
  applyCCMatch (InfixMatch _ p n ps rhs _) = case rhs of
    UnGuardedRhs _ e -> do
      x <- completeCase insideLet e
      return $ InfixMatch () p n ps (UnGuardedRhs () x) B.noBinds
    GuardedRhss _ _ -> error "applyCCMatch: GuardedRhs found"
