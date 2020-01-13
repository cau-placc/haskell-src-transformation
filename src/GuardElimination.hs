module GuardElimination ( eliminateL
                        , applyGEModule
                        , comp
                        , getMatchName) where                                   -- TODO Apply GE to GuardedRhs in case expressions
                                                                                -- TODO only apply to the parts with guards (not on matches if in case)
                                                                                    -- not false by semantics

import qualified Algo                         as A
import           Control.Monad
import           FreshVars
import qualified Language.Haskell.Exts.Build  as B
import           Language.Haskell.Exts.Syntax

type GExp = ([Pat ()], Rhs ())

-- Generates an expression with a let binding for each pattern + guard pair.
-- As defined in the semantics the first match of both pattern and guard has
-- to be evaluated causing a the sequential structure.
eliminateL :: [Pat ()]   -- fresh Vars
           -> Exp ()     -- error
           -> [GExp]     -- pairs of pattern and guarded rhs
           -> PM (Exp ())
eliminateL vs err eqs = do
  startVar <- A.newVar
  (decls, lastPat) <- foldGEqs vs ([],startVar) eqs
  let errDecl = toDecl lastPat err -- error has to be bound to last new var
  return  $ Let () (B.binds (errDecl:decls)) (A.translatePVar startVar)

toDecl:: Pat () -> Exp () -> Decl ()
toDecl p e = PatBind () p (UnGuardedRhs () e) B.noBinds

-- Folds the list of GExps to declarations.
foldGEqs :: [Pat ()]              -- fresh variables for the case exps
         -> ([Decl ()],Pat ())    -- startcase ([], first generated Pattern)
         -> [GExp]                -- list of pattern + rhs pair
         -> PM ([Decl ()],Pat ()) -- a list of declarations for the let binding and a free Variable for the error case
foldGEqs vs = foldM (\(decls,p) geq -> createDecl vs (decls,p) geq)

-- Generates a varbinding and a new variable for the next var binding
createDecl :: [Pat ()]                -- generated varibles
           -> ([Decl ()] , Pat ())    -- (current decls , variable for let binding)
           -> GExp                    -- pairs of pattern to match against and a guarded Rhs
           -> PM ([Decl ()] , Pat ()) -- var bindings , variable for next match
createDecl vs (decl,p) (ps,rhs) = do
  nVar <- A.newVar
  let varExp = A.translatePVar nVar
  iexp <- rhsToIf rhs varExp
  let cexp = createCase iexp varExp (zip vs ps)
  let ndecl = toDecl p cexp
  return (ndecl:decl, nVar)

-- TODO refactor to higher order
-- Generates a recursive case expression for each variable and pattern pair
createCase :: Exp ()             -- ifThenElse
           -> Exp ()             -- the other pattern (in case pattern match or guard fails)
           -> [(Pat (), Pat ())] -- Patterns to match (PVar , Pattern)
           -> Exp ()
--createCase i next vps  = foldr (\(v,p) next -> Case () (A.translatePVar v) [B.alt p res, B.alt B.wildcard next]) i vps
createCase i _    [] = i
createCase i next ((v,p):vps) = Case () (A.translatePVar v) [B.alt p (createCase i next vps) , B.alt B.wildcard next]

-- Converts a rhs into an if then else expression as mentioned in the semantics
rhsToIf :: Rhs ()      -- the (maybe guarded) righthandside
        -> Exp ()      -- next case
        -> PM (Exp ()) -- creates the if p_1 then . . . .
rhsToIf (UnGuardedRhs _ e)   _    = applyGEExp e
rhsToIf (GuardedRhss _ grhs) next = buildIF next grhs
 where
  buildIF :: Exp ()               -- next rule
          -> [GuardedRhs ()]      -- guarded rhs to fold
          -> PM (Exp ())          -- if then else expr
  buildIF nx gs = foldM (\res x -> (extract x) >>= \(e1 ,e2) -> return (If () e1 e2 res)) nx (reverse gs) -- reverse, since foldM is a foldl with side effect

-- Converts a guarded rhs into a pair of a boolean expression and the right side
extract :: GuardedRhs () -> PM (Exp (), Exp ())
extract (GuardedRhs _ [s] e)       = applyGEExp e >>= \a ->  return (fromQualifier s , a)
 where
  fromQualifier :: Stmt () -> Exp ()
  fromQualifier (Qualifier () qe) = qe
  fromQualifier _                 = error "fromQualifier: no Qualifier"
extract (GuardedRhs _ (_:_) _) = error "Currently only one guard exp allowed" -- TODO
extract (GuardedRhs _ [] _)        = error "GuardedRhss with no guards"

-- Applies guard elimination on an expression converting guarded rhs in cases
-- into unguarded exps
applyGEExp :: Exp () -> PM (Exp ())
applyGEExp e = case e of
  InfixApp _ e1 qop e2 -> do e1' <- applyGEExp e1
                             e2' <- applyGEExp e2
                             return $ InfixApp () e1' qop e2'
  App _ e1 e2          -> do e1' <- applyGEExp e1
                             e2' <- applyGEExp e2
                             return $ App () e1' e2'
  Lambda _ ps e1       -> do e' <- applyGEExp e1
                             return $ Lambda () ps e'
  Let _ bs e1          -> do e' <- applyGEExp e1
                             return $ Let () bs e'
  If _ e1 e2 e3        -> do e1' <- applyGEExp e1
                             e2' <- applyGEExp e2
                             e3' <- applyGEExp e3
                             return $ If () e1' e2' e3'
  Case _ e1 alts       -> do e'    <- applyGEExp e1
                             alts' <- applyGEAlts alts
                             return $ Case () e' alts'
  Tuple _ boxed es     -> do es' <- mapM applyGEExp es
                             return $ Tuple () boxed es'
  List _ es            -> do es' <- mapM applyGEExp es
                             return $ List () es'
  ListComp _ _ _       -> error "applyGEExp: ListComp not yet supported"
  x                    -> return x -- can cause problems if a exp is missing in this case

-- Applies guard elimination on alts by using eliminateL
applyGEAlts :: [Alt ()] -> PM ([Alt ()])
applyGEAlts as = if any (\(Alt _ _ rhs _) -> isGuardedRhs rhs ) as
                  then do
                    let gexps = map (\(Alt _ p rhs _) -> ([p],rhs)) as
                    newVar     <- A.newVar
                    e          <- eliminateL [newVar] A.err gexps
                    matchVar <- A.newVar
                    return [Alt () matchVar (UnGuardedRhs () e) B.noBinds]
                  else return as

-- Applies guard elimination to a module
applyGEModule :: Module () -> PM (Module ())
applyGEModule (Module _ mmh mps ids ds) = do dcls <- mapM applyGEDecl ds
                                             return $ Module () mmh mps ids dcls
applyGEModule _                         = error "applyGEModule: not on module"

-- Applies guard elimination to a declaration
applyGEDecl :: Decl () -> PM (Decl ())
applyGEDecl (FunBind _ ms ) = do nms <- applyGEMatches ms
                                 return (FunBind () nms)
applyGEDecl v               = return v

-- mapM
-- Applies guard elimination to a list of matches to generate one without guards
applyGEMatches :: [Match ()] -> PM ([Match ()])
applyGEMatches [] = return []
applyGEMatches (m:ms) = do
    let (oneFun, r) = span (comp m) ms
        funGroup = m:oneFun
    if hasGuards funGroup
     then do
       x  <- applyGE funGroup
       xs <- applyGEMatches r
       return (x:xs)
     else do
       xs <- applyGEMatches r
       return (funGroup ++ xs)

-- Applies guard elimination to one function
applyGE :: [Match ()] -- one fun group
        -> PM (Match ())
applyGE ms = do
  let mname = getMatchName ms
      geqs = map (\(Match _ _ pats rhs _) -> (pats,rhs)) ms
      funArity = (length . fst . head) geqs
  nVars <- A.newVars funArity
  nExp  <- eliminateL nVars A.err geqs
  return $ Match () mname nVars (UnGuardedRhs () nExp) Nothing

-- compares the names of two matches
comp :: Match () -> Match () -> Bool
comp (Match _ name _ _ _)        (Match _ name2 _ _ _)        = selectNameStr name == selectNameStr name2
comp (InfixMatch _ _ name _ _ _) (InfixMatch _ _ name2 _ _ _) = selectNameStr name == selectNameStr name2
comp _                           _                            = False

selectNameStr :: Name () -> String
selectNameStr (Ident _ str)  = str
selectNameStr (Symbol _ str) = str

getMatchName :: [Match ()] -> Name ()
getMatchName []                                = error "no match in getMatchName"
getMatchName ((Match _ mname _ _ _)        :_) = mname
getMatchName ((InfixMatch _ _ mname _ _ _) :_) = mname


-- A function which determines if a group of Matches contains GuardedRhs
hasGuards :: [Match ()] -- one function
          -> Bool
hasGuards = any hasGuards'
 where
  hasGuards' :: Match () -> Bool
  hasGuards' (Match _ _ _ rhs _)        = isGuardedRhs rhs
  hasGuards' (InfixMatch _ _ _ _ rhs _) = isGuardedRhs rhs

isGuardedRhs :: Rhs () -> Bool
isGuardedRhs (GuardedRhss _ _ ) = True
isGuardedRhs (UnGuardedRhs _ e) = containsGuardedRhsExp e

containsGuardedRhsExp :: Exp () -> Bool --TODO decide if guard is in matches or in matches
containsGuardedRhsExp e = case e of
  InfixApp _ e1 _ e2 -> (containsGuardedRhsExp) e1 || (containsGuardedRhsExp e2)
  App _ e1 e2        -> (containsGuardedRhsExp) e1 || (containsGuardedRhsExp e2)
  Lambda _ _ e'      -> containsGuardedRhsExp e'
  Let _ _ e'         -> containsGuardedRhsExp e'
  If _ e1 e2 e3      -> any containsGuardedRhsExp [e1,e2,e3]
  Case _ e' alts     -> (containsGuardedRhsExp e') || any containsGuardedRhsAlt alts
  Tuple _ _ es       -> any containsGuardedRhsExp es
  List _ es          -> any containsGuardedRhsExp es
  ListComp _ _ _     -> error "containsGuardedRhsExp: ListComp not yet supported"
  _             -> False

containsGuardedRhsAlt :: Alt () -> Bool
containsGuardedRhsAlt (Alt _ _ rhs _) = isGuardedRhs rhs
