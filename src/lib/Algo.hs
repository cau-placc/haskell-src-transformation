module Algo
  ( match
  , err
  , optimize
  , translatePVar
  , newVars
  , newVar
  , Eqs
  , isPVar
  , isCons
  )
where

import           Data.List
import           FreshVars
import           Language.Haskell.Exts
import qualified Language.Haskell.Exts.Build   as B
import           Renaming

-- |The type 'Eqs' is a pair containing a list of pattern and an expression.
-- Usually called eqation throughout the comments
type Eqs = ([Pat ()], Exp ())

-- |The 'undefined'-error encapsulated in an expression
err :: Exp ()
err = Var () (UnQual () (Ident () "undefined"))

-- |The 'match' function compiles patternmatching into complete case expressions.
match
  :: [Pat ()]             -- list of variables (variable Pattern) fresh vars
  -> [Eqs]                -- list of equations (Eqs)
  -> Exp ()               -- error or alternative computation
  -> PM (Exp ())          -- resulting right hand side
match [] (([], e) : _) _  = return e              -- Rule 3a
match [] []            er = return er             -- Rule 3b
match vars@(x : xs) eqs er
  | allVars eqs = do
    eqs' <- mapM (substVars x) eqs
    match xs eqs' er
  |  -- Rule 1
    allCons eqs = makeRhs x xs eqs er
  |                   -- Rule 2
    otherwise = createRekMatch vars er (groupPat eqs) -- Rule 4
match [] _ _ = error "match with non-fitting args"

-- | The 'substVars' function takes a variable 'pv' and a pair of a list of
-- pattern and an expression and renames all occurences of the first variable from
-- the list in the expression into 'pv'.
substVars :: Pat () -> Eqs -> PM Eqs  -- Variable Pattern
substVars pv (p : ps, e) = do
  s1 <- getPVarName p
  s2 <- getPVarName pv
  let sub = subst s1 s2
  return (ps, rename sub e)
substVars _ _ = error "more variables than eqs"


-- | The function 'getPvarName' takes a pattern andreturns the name of the variable
-- pattern. In case that the pattern is a wildcard the function returns a name
-- for a fresh variable.
-- The function returns an error if the Pattern is not a wildcard or variable.
getPVarName :: Pat () -> PM String
getPVarName (PVar _ pname) = getNameStr pname
 where
  getNameStr (Ident  _ str) = return str
  getNameStr (Symbol _ str) = return str
getPVarName (PWildCard _) = do
  n <- freshVar
  return $ 'a' : show n
getPVarName x = error $ "getPVarName: tried to get Pattern " ++ show x

-- | The function 'groupPat' grouped  a list of equations by the first pattern
-- resulting in alternating groups where the first pattern starts with either a
-- variable or a constructor.
-- The grouping is stable.
groupPat :: [Eqs] -> [[Eqs]]
groupPat = groupBy ordPats
  where ordPats (x, _) (y, _) = isPVar (head x) && isPVar (head y)

isPVar :: Pat () -> Bool
isPVar (PVar _ _) = True
isPVar _          = False

-- |The function 'createRekMatch' folds a given alternatin group of equations into
-- one rekursive call of match. For each group the error of the match call is the
-- match call of the next group.
createRekMatch
  :: [Pat ()]                   -- list of variables to match
  -> Exp ()                     -- error
  -> [[Eqs]]                    -- alternating groups
  -> PM (Exp ())                -- result
createRekMatch vars er =
  foldr (\eqs mrhs -> mrhs >>= match vars eqs) (return er)

-- |The function 'makeRhs' creates a case expression.
-- It takes a fresh variable for the case expression, more fresh variables for
-- the rekursive call of the match function, a list of equations and an
-- expression containing an error.
makeRhs
  :: Pat ()               -- fresh variable for case expression
  -> [Pat ()]             -- list of fresh variables which are to match
  -> [Eqs]                -- list of tuples of pattern and right sides
  -> Exp ()               -- error
  -> PM (Exp ())          -- resulting right hand side
makeRhs x xs eqs er = do
  alts <- computeAlts x xs eqs er
  return (caseE (translatePVar x) alts)

-- | The function 'translatePVar' converts a given PVar into a variable expression.
translatePVar :: Pat () -> Exp ()
translatePVar (PVar _ vname) = B.var vname
translatePVar _              = error "no PVar in translatePVar"

                                                                                -- TODO remove redundand types
-- | The function 'computeAlts' returns a list of alternatives for a case expression.
-- It also looks up the missing constructors and and checks if trivial case completion
-- is enabled.
computeAlts
  :: Pat ()          -- variable
  -> [Pat ()]        -- to match variables
  -> [Eqs]           -- list of eqs for recursive match call
  -> Exp ()          -- error
  -> PM [Alt ()]
computeAlts x xs eqs er = do
  alts <- mapM (computeAlt x xs er) (groupByCons eqs)
  mxs  <- getMissingConstrs alts
  case mxs of
    [] -> return alts
    zs -> do
      b <- gets trivialCC
      if b
        then return $ alts ++ [B.alt B.wildcard err]
        else do
          z <- createAltsFromConstr x zs er
          return $ alts ++ z                                    -- TODO currently not sorted (reversed)

-- | The function 'getMissingConstrs' is a reverse lookup if the constructorname
-- from the given alts in the constrMap of the state PM.
-- Takes a list of case alternatives and returns all missing constructors.
getMissingConstrs :: [Alt ()] -> PM [Constructor]
getMissingConstrs []   = error "getMissingConstrs on empty list"
getMissingConstrs alts = do
  cmap <- gets constrMap                     -- [datantype, (constructor,arity)]
  let names     = map getQName alts                           -- constructornames
      (_, cons) = findDataType (head names) cmap
  return (findCons cons names)

-- | The function 'findCons' takes a list of cunstructors and a list of
-- names and returns all constructors which names are not part of the list of names.
findCons
  :: [Constructor]                                                       -- TODO rename // complement
  -> [QName ()]
  -> [Constructor]
findCons cons usedcons =
  filter (\con -> getConstrName con `notElem` usedcons) cons

-- | The function 'findDataType' takes a constructor name and a constructor map
-- and returns the datatype for the constructor if the type is part of the map.
findDataType
  :: QName ()                   -- Constructor name
  -> [(String, [Constructor])]  -- [(Datatype, [(Konstruktor,Arität)])]
  -> (String, [Constructor])     -- Datatype
findDataType cname = foldr
  (\c sc -> if cname `elem` map getConstrName (snd c) then c else sc)
  (error "no data Type") -- todo fst und snd als synonym

-- | The function 'getQName' returns the name of the constructor from a case alternative.
getQName :: Alt () -> QName ()
getQName (Alt _ p _ _) = getQNamePat p
-- getQName  _             = error "getQName: expected an Alt"

-- | The function 'getQNamePat' the name of a given constructor or wildcard pattern
getQNamePat :: Pat () -> QName ()
getQNamePat (PApp _ qn _       ) = qn
getQNamePat (PInfixApp _ _ qn _) = qn
getQNamePat (PList _ _         ) = Special () (ListCon ())
getQNamePat (PWildCard _       ) = Special () (ExprHole ())
getQNamePat (PTuple _ bxd ps   ) = Special () (TupleCon () bxd (length ps))
getQNamePat _                    = error "getQNamePat unsuported Pattern"

                                                                                -- TODO refactor with smartcons
-- |The function 'createAltsFromConstr' fills the missing constructors with
-- new variables and returns them.
createAltsFromConstr
  :: Pat ()        -- variable from case exp
  -> [Constructor] -- missing sonstructors
  -> Exp ()        -- error
  -> PM [Alt ()]
createAltsFromConstr x cs er = mapM (createAltFromConstr x er) cs
 where
  createAltFromConstr :: Pat () -> Exp () -> Constructor -> PM (Alt ())
  createAltFromConstr pat e (qn, ar, b) = do
    nvars@[nvar0, nvar1] <- newVars ar
    let p | b         = PInfixApp () nvar0 qn nvar1
          | otherwise = PApp () qn nvars
        p'   = translateApp p
        pat' = translatePVar pat
        e'   = substitute (tSubst pat' p') e
    return (B.alt p e')

-- |The function newVars generates a list of new Variables with IDs from
-- the State. Takes an Integer to determine how many variables are generated.
newVars :: Int -> PM [Pat ()]
newVars 0 = return []
newVars n = do
  nvar <- newVar
  vs   <- newVars (n - 1)
  return (nvar : vs)

-- |The function newVar generates a single new variable with ID from
-- the State.
newVar :: PM (Pat ())
newVar = do
  nv <- freshVar
  let v = 'a' : show nv
  return (B.pvar (name v))

-- | The function 'groupByCons' groups a list of equations into a list of alternating
-- groups where in each group the equations first pattern is a variable or a constructor pattern.
-- The grouping is stable, meaning that the order of "equal" eqs does not change.
groupByCons :: [Eqs] -> [[Eqs]]
groupByCons = groupBy2 select

-- | The function 'groupBy2' is a self defined groupBy but the grouping stayes stable.
-- Example: "Mississippi" results into ["M","i","ss","i","ss","i","pp","i"]
-- when grouped by vocals and consonants.
groupBy2 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy2 = groupBy2' []
 where
  groupBy2' :: [[a]] -> (a -> a -> Bool) -> [a] -> [[a]]
  groupBy2' acc _ [] = reverse acc               --TODO makes acc redundant.. refactor
  groupBy2' acc comp (x : xs) =
    let (ys, zs) = partition (comp x) xs in groupBy2' ((x : ys) : acc) comp zs

-- | The function 'select' compares two equations by comparing their first pattern.
select :: Eqs -> Eqs -> Bool
select (x, _) (y, _) = compareCons (head x) (head y)

-- | The function 'compareCons' compares 2 given pattern and returns if they are
-- both constructor pattern or wildcards.
compareCons :: Pat () -> Pat () -> Bool
compareCons (PApp _ qn1 _       ) (PApp _ qn2 _       ) = qn1 == qn2
compareCons (PInfixApp _ _ qn1 _) (PInfixApp _ _ qn2 _) = qn1 == qn2
compareCons (PApp _ qn1 _       ) (PInfixApp _ _ qn2 _) = qn1 == qn2
compareCons (PInfixApp _ _ qn1 _) (PApp _ qn2 _       ) = qn1 == qn2
compareCons (PParen _ p1        ) p2                    = compareCons p1 p2
compareCons p1                    (PParen _ p2   )      = compareCons p1 p2
compareCons (PList _ ps1) (PList _ ps2) = length ps1 == length ps2        -- TODO Special Syntax
compareCons (PWildCard _   )      (PWildCard _   )      = True
compareCons (PTuple _ _ ps1) (PTuple _ _ ps2) = length ps1 == length ps2
compareCons _                     _                     = False

-- | The function 'computeAlt' calls match recursively with one group of
-- eqs and then substitutes the variable from the case expression
-- in all results to keep linearity of transformed functions.
computeAlt
  :: Pat ()               -- variable from case exp
  -> [Pat ()]             -- variables to match
  -> Exp ()               -- error
  -> [Eqs]                -- one group to match
  -> PM (Alt ())          -- one group
computeAlt _   _    _  []           = error "computeAlt with empty eqs"
computeAlt pat pats er prps@(p : _) = do
  (capp, nvars, _) <- getConst (firstPat p)  -- oldpats need to be computed for each pattern
  nprps            <- mapM f prps
  let sub = tSubst (translatePVar pat) (translateApp capp)
  res <- match (nvars ++ pats) nprps (substitute sub er)
  let res' = substitute sub res
  return (B.alt capp res')
 where
  f :: Eqs -> PM Eqs
  f ([]    , r) = return ([], r) -- potentially unused
  f (v : vs, r) = do
    (_, _, oldpats) <- getConst v
    return (oldpats ++ vs, r)

-- |The function 'translateApp' takes a pattern representing a constructor application
-- and transforms it into an expression with the same constructor applied to translated
-- variables.
translateApp :: Pat () -> Exp ()
translateApp (PApp _ qn ps) =
  foldl (\acc x -> App () acc (translatePVar x)) (Con () qn) ps
translateApp (PInfixApp _ p1 qn p2) =
  InfixApp () (translatePVar p1) (QConOp () qn) (translatePVar p2)
translateApp (PTuple _ bxd ps) = Tuple () bxd $ map translatePVar ps
translateApp (PList _ ps) = List () $ map translatePVar ps
translateApp pat = error ("translateApp does not support: " ++ show pat)

-- TODO refactor into 2 functions. one for the capp and nvars and one for the oldpats
-- |The function 'getConst' takes a constructor or wildcard pattern  and returns a triple
-- containing a constructor pattern with a the same constructor but filled with fresh variables,
-- a list containing the fresh variables and a list containing the "old" variables.
getConst :: Pat () -> PM (Pat (), [Pat ()], [Pat ()])
getConst (PApp _ qname ps) = do
  nvars <- newVars (length ps)
  return (PApp () qname nvars, nvars, ps)
getConst (PInfixApp _ p1 qname p2) = do
  nvars <- newVars 2
  let [nv1, nv2] = nvars
      ps         = [p1, p2]
  return (PInfixApp () nv1 qname nv2, nvars, ps)
getConst (PParen _ p) = getConst p
getConst (PList _ ps)
  | null ps = return (PList () [], [], [])
  | otherwise = do
    let (n : nv) = ps
        listCon  = Special () $ Cons ()
    getConst (PInfixApp () n listCon (PList () nv))
getConst (PTuple _ bxd ps) = do
  nvars <- newVars (length ps)
  return (PTuple () bxd nvars, nvars, ps)
getConst (PWildCard _) = return (PWildCard (), [], [])              -- wildcards no longer needed as cons
getConst _             = error "wrong Pattern in getConst"

-- |The function 'allVars' determines if every pattern list in a list of equations
-- starts with a variable pattern.
allVars :: [Eqs] -> Bool
allVars = all (isVar . firstPat)

-- |The function 'firstPat' is a selector returning the first pattern of a list
-- of pattern from an equation.
firstPat :: Eqs -> Pat ()
firstPat = head . fst

-- |The function 'isVar' returns true if the pattern is a variable or wildcard pattern
isVar :: Pat () -> Bool
isVar (PVar _ _   ) = True
isVar (PWildCard _) = True
isVar _             = False

-- |The function 'allCons' determines if every pattern list in a list of equations
-- starts with a constructor pattern.
allCons :: [Eqs] -> Bool
allCons = all (isCons . firstPat)

-- |The function 'isCons' returns true if the pattern is a constructor pattern
-- including list and tuple pattern.
isCons :: Pat () -> Bool
isCons p = case p of
  PApp _ _ _        -> True
  PInfixApp _ _ _ _ -> True
  PParen _ p'       -> isCons p'
  PList  _ _        -> True
  PTuple _ _ _      -> True
  PWildCard _       -> False -- Wildcards are now treated as variables
  _                 -> False

--------------------------------------------------------------------------------
{- OPTIMIZATION -}
--------------------------------------------------------------------------------

-- |The 'optimize' function takes an expression and removes all case expressions that
-- match on the same variable.
optimize :: Exp () -> PM (Exp ())
optimize ex = case ex of
  InfixApp _ e1 qop e2 -> do
    e1' <- optimize e1
    e2' <- optimize e2
    return $ InfixApp () e1' qop e2'
  App _ e1 e2 -> do
    e1' <- optimize e1
    e2' <- optimize e2
    return $ App () e1' e2'
  Lambda _ ps e -> do
    e' <- optimize e
    return $ Lambda () ps e'
  Let _ b e -> do
    e' <- optimize e
    return $ Let () b e'
  If _ e1 e2 e3 -> do
    e1' <- optimize e1
    e2' <- optimize e2
    e3' <- optimize e3
    return $ If () e1' e2' e3'
  Case _ e alts  -> optimizeCase e alts
  Do _ _         -> error "optimize : do is not supported"
  Tuple _ bxd es -> do
    es' <- mapM optimize es
    return $ Tuple () bxd es'
  List _ es -> do
    es' <- mapM optimize es
    return $ List () es'
  Paren _ e -> do
    e' <- optimize e
    return $ Paren () e'
  c -> return c

-- |The function 'optimizeCase' checks if an expression e is a variable and
-- if it is  already bound to a pattern. If the variable is bound the case expression
-- gets replaced by the case alternative with the same pattern. Otherwise the pattern
-- is sequentially bound to all constructors and then the alts are optimized.
optimizeCase :: Exp () -> [Alt ()] -> PM (Exp ())
optimizeCase e alts
  | isVarExp e = do
    mpats <- gets matchedPat
    case lookup e mpats of                  -- lookupBy ?
      Just pat -> renameAndOpt pat alts  -- look for the correct pattern replace, case exp and rename
      Nothing  -> addAndOpt e alts
  |      -- stackwise add it to first place and then remove first
    otherwise = do
    e'    <- optimize e
    alts' <- optimizeAlts alts
    return $ Case () e' alts'

isVarExp :: Exp () -> Bool
isVarExp (Var _ _) = True
isVarExp _         = False

-- TODO generalise
-- |The function 'renameAndOpt' takes a  pattern and
-- a list of case alternatives and checks if there is an alternative with
-- same constructor name as the matched one. In case one is found the
-- variables in the right side of the found one are replaced with the already
-- matched ones.
-- After that the expression is further optimized with the 'optimize' function.
renameAndOpt
  :: Pat ()      -- found Pattern for variable
  -> [Alt ()]    -- alternatives from the case exp
  -> PM (Exp ())
renameAndOpt pat alts =
  let aPaR     = map (\(Alt _ p r _) -> (p, r)) alts
      patQ     = getQNamePat pat
      sameCons = filter (\(p, _) -> cheatEq (getQNamePat p) patQ) aPaR
  in  case sameCons of
        [] ->
          error
            $  "Found in case but not found in alts : Tried"
            ++ show patQ
            ++ " Searched in "
            ++ show (map fst aPaR)
        ((p, r) : _) -> do
          let e  = selectExp r
              p1 = selectPats pat
              p2 = selectPats p
          res <- renameAll (zip p2 p1) e  -- Fixes the renaming bug -> was p1 p2 before
          optimize res



-- |The function 'cheatEq' is an alternative compare function for QNames.
-- It is equal to the EQ instance for QName but the Symbol and Ident constructors
-- are ignored and only the strings are compared.
-- Ident "+:" is not equal to Symbol "+:"
cheatEq :: QName () -> QName () -> Bool
cheatEq (UnQual () (Symbol () s1)) (UnQual () (Ident  () s2)) = s1 == s2
cheatEq (UnQual () (Ident  () s1)) (UnQual () (Symbol () s2)) = s1 == s2
cheatEq q1                         q2                         = q1 == q2

-- |The function 'selectPats' selects all patterns from a constructor pattern
selectPats :: Pat () -> [Pat ()]
selectPats (PApp _ _ pats) = pats
selectPats (PInfixApp _ p1 _ p2) = [p1, p2]
selectPats p = error $ "selectPat: not definied for " ++ show p

-- |The function 'selectExp' selects an expression from a given unguarded righthandside
selectExp :: Rhs () -> Exp ()
selectExp (UnGuardedRhs _ e) = e
selectExp _                  = error "selectExp: only unguarded rhs"

-- |The function 'renameAll' takes a list of pairs of pattern and an expression.
-- Then it renames all first pattern into the second pattern in the expression.
renameAll :: [(Pat (), Pat ())] -> Exp () -> PM (Exp ()) -- TODO refactor higher order foldr -- generate one Subst and apply only once
renameAll []               e = return e
renameAll ((from, to) : r) e = do
  f   <- getPVarName from
  t   <- getPVarName to
  res <- renameAll r e
  return $ rename (subst f t) res

-- |The function 'addAndOpt' takes and expression and a list of case alternatives
addAndOpt :: Exp () -> [Alt ()] -> PM (Exp ()) -- for each add a binding -> optimize right side -> remove binding
addAndOpt e alts = do
  alts' <- mapM (bindAndOpt e) alts
  return $ Case () e alts'
 where
  bindAndOpt :: Exp () -> Alt () -> PM (Alt ()) -- uses the list of Exp Pat as a stack
  bindAndOpt v a@(Alt _ p _ _) = do
    stack <- gets matchedPat
    modify $ \state -> state { matchedPat = (v, p) : stack }
    alt' <- optimizeAlt a
    modify $ \state -> state { matchedPat = stack }
    return alt'


optimizeAlts :: [Alt ()] -> PM [Alt ()]
optimizeAlts = mapM optimizeAlt

-- |The function 'optimizeAlt' takes a case alternative and optimizes its
-- right hand side expression.
optimizeAlt :: Alt () -> PM (Alt ())
optimizeAlt (Alt _ p rhs _) = do
  let (UnGuardedRhs _ e) = rhs
  e' <- optimize e
  return $ Alt () p (UnGuardedRhs () e') B.noBinds
