-- | This module contains the actual implementation of the pattern-matching
--   compilation algorithm.

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

-- | A type that represents a single equation of a function declaration.
--
--   An equation is characterized by the argument patterns and the right-hand
--   side.
type Eqs = ([Pat ()], Exp ())

-- | The default error expression to insert for pattern matching failures.
err :: Exp ()
err = Var () (UnQual () (Ident () "undefined"))

-- | Compiles the given equations of a function declaration to a single
--   expression that performs explicit pattern matching using @case@
--   expressions.
--
--   All equations must have the same number of patterns as the given list
--   of fresh variable patterns.
match
  :: [Pat ()] -- ^ Fresh variable patterns.
  -> [Eqs]    -- ^ The equations of the function declaration.
  -> Exp ()   -- ^ The error expression for pattern-matching failures.
  -> PM (Exp ())
match [] (([], e) : _) _  = return e  -- Rule 3a: All patterns matched.
match [] []            er = return er -- Rule 3b: Pattern-matching failure.
match vars@(x : xs) eqs er
  | -- Rule 1: Pattern list of all equations starts with a variable pattern.
    allVars eqs = do
    eqs' <- mapM (substVars x) eqs
    match xs eqs' er
  | -- Rule 2: Pattern lists of all equations starts with a constructor pattern.
    allCons eqs = makeRhs x xs eqs er
  | -- Rule 4: Pattern lists of some equations start with a variable pattern
    -- and others start with a constructor pattern.
    --
    -- TODO This is probably causing the infinite loops when there are
    -- unsupported patterns.
    otherwise = createRekMatch vars er (groupPat eqs)
match [] _ _ = error "match: equations have different number of arguments"

-- | Substitutes all occurrences of the variable bound by the first pattern
--   (must be a variable or wildcard pattern) of the given equation by the
--   fresh variable bound by the given variable pattern on the right-hand side
--   of the equation.
substVars :: Pat () -> Eqs -> PM Eqs
substVars pv (p : ps, e) = do
  s1 <- getPVarName p
  s2 <- getPVarName pv
  let sub = subst s1 s2
  return (ps, rename sub e)
substVars _ _ = error "substVars: expected equation with at least one pattern"

-- | Extracts the name of the variable bound by the given variable pattern.
--
--   Returns a fresh variable for wildcard patterns.
--   The given pattern must be a variable or wildcard pattern.
getPVarName :: Pat () -> PM String
getPVarName (PVar _ pname) = getNameStr pname
 where
  getNameStr (Ident  _ str) = return str
  getNameStr (Symbol _ str) = return str
getPVarName (PWildCard _) = do
  n <- freshVar
  return $ 'a' : show n
getPVarName x =
  error $ "getPVarName: expected variable or wildcard pattern, got " ++ show x

-- | Groups the given equations based on the type of their first pattern.
--
--   The order of the equations is not changed, i.e., concatenation of the
--   resulting groups yields the original list of equations.

--   Two equations are in the same group only if their pattern lists both
--   start with a variable pattern or both start with a constructor pattern.
groupPat :: [Eqs] -> [[Eqs]]
groupPat = groupBy ordPats
  where ordPats (ps, _) (qs, _) = isPVar (head ps) && isPVar (head qs)

-- | Tests whether the given pattern is a variable pattern.
--
--   TODO shouldn't this return @True@ for wildcard patterns, too?
--   What's the difference to @isVar@?
isPVar :: Pat () -> Bool
isPVar (PVar _ _) = True
isPVar _          = False

-- | Applies 'match' to every group of equations where the error expression
--   is the 'match' result of the next group.
createRekMatch
  :: [Pat ()] -- ^ Fresh variable patterns.
  -> Exp ()   -- ^ The error expression for pattern-matching failures.
  -> [[Eqs]]  -- ^ Groups of equations (see 'groupPat').
  -> PM (Exp ())
createRekMatch vars er =
  foldr (\eqs mrhs -> mrhs >>= match vars eqs) (return er)

-- | Creates a case expression that performs pattern matching on the variable
--   bound by the given variable pattern.
makeRhs
  :: Pat ()   -- ^ The fresh variable pattern to match.
  -> [Pat ()] -- ^ The remaing fresh variable patterns.
  -> [Eqs]    -- ^ The equations.
  -> Exp ()   -- ^ The error expression for pattern-matching failures.
  -> PM (Exp ())
makeRhs x xs eqs er = do
  alts <- computeAlts x xs eqs er
  return (caseE (translatePVar x) alts)

-- | Converts the given variable pattern to a variable expression.
translatePVar :: Pat () -> Exp ()
translatePVar (PVar _ vname) = B.var vname
translatePVar p =
  error ("translatePVar: expected variable pattern, got" ++ show p)

-- TODO remove redundand types

-- | Generates @case@ expression alternatives for the given equations.
--
--   If there are missing constructors and trivial case completion is enabled,
--   an alternative with an wildcard pattern is added.
--   If trivial case completion is not enabled, one alternative is added for
--   every missing constructor.
computeAlts
  :: Pat ()   -- ^ The variable pattern that binds the matched variable.
  -> [Pat ()] -- ^ The remaing fresh variable patterns.
  -> [Eqs]    -- ^ The equations to generate alternatives for.
  -> Exp ()   -- ^ The error expression for pattern-matching failures.
  -> PM [Alt ()]
computeAlts x xs eqs er = do
  alts <- mapM (computeAlt x xs er) (groupByCons eqs)
  mxs  <- getMissingConstrs alts
  case mxs of
    [] -> return alts
    zs -> do
      b <- gets trivialCC
      if b
        then -- TODO is 'err' correct? Why not 'er'?
             return $ alts ++ [B.alt B.wildcard err]
        else do
          z <- createAltsFromConstr x zs er
          -- TODO currently not sorted (reversed)
          return $ alts ++ z

-- | Looks up the constructors of the data type that is matched by the given
--   @case@ expression alternatives for which there are no alternatives already.
getMissingConstrs :: [Alt ()] -> PM [Constructor]
getMissingConstrs []   = error "getMissingConstrs: empty list"
getMissingConstrs alts = do
  cmap <- gets constrMap -- [(datantype, (constructor,arity))]
  let names     = map getQName alts -- Names of matched constructors.
      (_, cons) = findDataType (head names) cmap
  return (findCons cons names)

-- | Removes the 'Constructor's with the given names from the given list.
findCons
  :: [Constructor]  -- TODO rename // complement
  -> [QName ()]
  -> [Constructor]
findCons cons usedcons =
  filter (\con -> getConstrName con `notElem` usedcons) cons

-- | Looks up the data type for the given constructor in the given map.
findDataType
  :: QName ()                   -- Constructor name
  -> [(String, [Constructor])]  -- [(Datatype, [(Konstruktor,Arität)])]
  -> (String, [Constructor])     -- Datatype
findDataType cname = foldr
  (\c sc -> if cname `elem` map getConstrName (snd c) then c else sc)
  (error
    ("findDataType: could not find data type for constructor " ++ show cname)
  ) -- todo fst und snd als synonym

-- | Gets the name of the constructor matched by the given @case@ expression
--   alternative.
getQName :: Alt () -> QName ()
getQName (Alt _ p _ _) = getQNamePat p
-- getQName  _             = error "getQName: expected an Alt"

-- | Gets the name of a constructor pattern.
--
--   Returns the 'Special' names for special patterns such as lists and tuples.
getQNamePat :: Pat () -> QName ()
getQNamePat (PApp _ qn _       ) = qn
getQNamePat (PInfixApp _ _ qn _) = qn
getQNamePat (PList _ _         ) = Special () (ListCon ())
getQNamePat (PWildCard _       ) = Special () (ExprHole ()) -- TODO aren't wildcard pattern considers variable and not constructor patterns?
getQNamePat (PTuple _ bxd ps   ) = Special () (TupleCon () bxd (length ps))
getQNamePat _                    = error "getQNamePat unsuported Pattern"

-- TODO refactor with smartcons

-- | Creates new @case@ expression alternatives for the given missing
--   constructors.
createAltsFromConstr
  :: Pat ()        -- ^ The fresh variable matched by the @case@ expression.
  -> [Constructor] -- ^ The missing constructors to generate alternatives for.
  -> Exp ()        -- ^ The error expression for pattern-matching failures.
  -> PM [Alt ()]
createAltsFromConstr x cs er = mapM (createAltFromConstr x er) cs
 where
  createAltFromConstr :: Pat () -> Exp () -> Constructor -> PM (Alt ())
  createAltFromConstr pat e (qn, ar, b) = do
    nvars <- newVars ar
    let p | b         = PInfixApp () (head nvars) qn (nvars !! 1)
          | otherwise = PApp () qn nvars
        p'   = translateApp p
        pat' = translatePVar pat
        e'   = substitute (tSubst pat' p') e
    return (B.alt p e')

-- | Generates the given number of fresh variables.
--
--   The generated variables use IDs from the state.
newVars :: Int -> PM [Pat ()]
newVars 0 = return []
newVars n = do
  nvar <- newVar
  vs   <- newVars (n - 1)
  return (nvar : vs)

-- | Generates a single fresh variable with an ID from the state.
newVar :: PM (Pat ())
newVar = do
  nv <- freshVar
  let v = 'a' : show nv
  return (B.pvar (name v))

-- | Groups the given equations based on the constructor matched by their
--   first pattern.
--
--   The order of equations that match the same constructor (i.e., within
--   groups) is preserved.
groupByCons :: [Eqs] -> [[Eqs]]
groupByCons = groupBy2 select

-- | A version of 'groupBy' that produces only one group for all
--   elements that are equivalent under the given predicate.
--
--   For example
--
--   > groupBy2 ((==) `on` fst)
--   >          [('a', 1), ('a', 2), ('b', 3), ('a', 4), ('b', 5)]
--
--   yields the following groups
--
--   > [[('a',1),('a',2),('a',4)],[('b',3),('b',5)]]
--
--   whereas 'groupBy' would have returned the following.
--
--   > [[('a',1),('a',2)],[('b',3)],[('a',4)],[('b',5)]]
--
--   This function is stable as the order of elements within each group
--   is preserved.
groupBy2 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy2 = groupBy2' []
 where
  groupBy2' :: [[a]] -> (a -> a -> Bool) -> [a] -> [[a]]
  groupBy2' acc _ [] = reverse acc -- TODO makes acc redundant.. refactor
  groupBy2' acc comp (x : xs) =
    let (ys, zs) = partition (comp x) xs in groupBy2' ((x : ys) : acc) comp zs

-- | Tests whether the pattern lists of the given equations start with the same
--   constructor.
select :: Eqs -> Eqs -> Bool
select (ps, _) (qs, _) = compareCons (head ps) (head qs)

-- | Tests whether the given patterns match the same constructor or both match
--   the wildcard pattern.
compareCons :: Pat () -> Pat () -> Bool
compareCons (PApp _ qn1 _       ) (PApp _ qn2 _       ) = qn1 == qn2
compareCons (PInfixApp _ _ qn1 _) (PInfixApp _ _ qn2 _) = qn1 == qn2
compareCons (PApp _ qn1 _       ) (PInfixApp _ _ qn2 _) = qn1 == qn2
compareCons (PInfixApp _ _ qn1 _) (PApp _ qn2 _       ) = qn1 == qn2
compareCons (PParen _ p1        ) p2                    = compareCons p1 p2
compareCons p1                    (PParen _ p2   )      = compareCons p1 p2
-- TODO Special Syntax
compareCons (PList _ ps1) (PList _ ps2) = length ps1 == length ps2
compareCons (PWildCard _   )      (PWildCard _   )      = True
compareCons (PTuple _ _ ps1) (PTuple _ _ ps2) = length ps1 == length ps2
compareCons _                     _                     = False

-- | Creates an alternative for a @case@ expression for the given group of
--   equations whose first pattern matches the same constructor.
--
--   The variable matched by the case expression is substituted by the
--   expression that corresponds to the pattern of the alternative
--   in order to preserve "linearity". This is an optimization that was
--   originally implemented for the free-compiler to compensate the
--   lack of sharing. This optimization turned out to confuse the
--   termination check of the free-compiler and can probably be removed
--   once sharing is implemented.
computeAlt
  :: Pat ()   -- ^ The variable pattern that binds the matched variable.
  -> [Pat ()] -- ^ The remaing fresh variable patterns.
  -> Exp ()   -- ^ The error expression for pattern-matching failures.
  -> [Eqs]    -- ^ A group of equations (see 'groupByCons').
  -> PM (Alt ())
computeAlt _   _    _  []           = error "computeAlt: no equations"
computeAlt pat pats er prps@(p : _) = do
  -- oldpats need to be computed for each pattern
  (capp, nvars, _) <- getConst (firstPat p)
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

-- | Converts a constructor application pattern (where the argument patterns
--   are variable or wildcard patterns) to an expression.
translateApp :: Pat () -> Exp ()
translateApp (PApp _ qn ps) =
  foldl (\acc x -> App () acc (translatePVar x)) (Con () qn) ps
translateApp (PInfixApp _ p1 qn p2) =
  InfixApp () (translatePVar p1) (QConOp () qn) (translatePVar p2)
translateApp (PTuple _ bxd ps) = Tuple () bxd $ map translatePVar ps
translateApp (PList _ ps) = List () $ map translatePVar ps
translateApp pat = error ("translateApp does not support: " ++ show pat)

-- TODO refactor into 2 functions. one for the capp and nvars and one for the
-- oldpats

-- | Replaces the child patterns of a constructor pattern with fresh variable
--   patterns.
--
--   Returns the constructor with replaced child patterns and a list of
--   new and old variable patterns.
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
-- wildcards no longer needed as cons
getConst (PWildCard _) = return (PWildCard (), [], [])
getConst _             = error "wrong Pattern in getConst"

-- | Tests whether the pattern lists of all given equations starts with a
--   variable pattern.
allVars :: [Eqs] -> Bool
allVars = all (isVar . firstPat)

-- | Gets the first pattern of the pattern list of the given equation.
firstPat :: Eqs -> Pat ()
firstPat = head . fst

-- | Tests whether the given pattern is a variable or wildcard pattern.
isVar :: Pat () -> Bool
isVar (PVar _ _   ) = True
isVar (PWildCard _) = True
isVar _             = False

-- | Tests whether the pattern lists of all given equations starts with a
--   constructor pattern.
allCons :: [Eqs] -> Bool
allCons = all (isCons . firstPat)

-- | Tests whether the given pattern is a constructor pattern.
--
--   Special patterns for lists and tuples are also considered constructor
--   patterns.
isCons :: Pat () -> Bool
isCons p = case p of
  PApp _ _ _        -> True
  PInfixApp _ _ _ _ -> True
  PParen _ p'       -> isCons p'
  PList  _ _        -> True
  PTuple _ _ _      -> True
  PWildCard _       -> False -- Wildcards are now treated as variables
  _                 -> False

-------------------------------------------------------------------------------
-- Optimization                                                              --
-------------------------------------------------------------------------------

-- | Removes all case expressions that are nested inside another case
--   expression for the same variable.
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

-- | Tests whether the given scrutinee of a @case@ expression is a variable
--   that has already been matched by a surrounding @case@ expression.
--
--   If the scrutinee is a variable that has been matched already, the
--   current @case@ expression is redundant and the appropriate alternative
--   can be selected directly.
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

-- | Tests whether the given expression is a variable expression.
isVarExp :: Exp () -> Bool
isVarExp (Var _ _) = True
isVarExp _         = False

-- TODO generalise

-- | Gets the right-hand side of the alternative that matches the same
--   constructor as the given pattern, renames variable patterns in the
--   alternative to the names of the corresponding variable patterns of the
--   given pattern and applies 'optimize'.
renameAndOpt
  :: Pat () -- ^ A pattern of a parent @case@ expression on the same scrutinee.
  -> [Alt ()] -- ^ The alternatives of the current @case@ expression.
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

-- | Compares the given 'QName's ignoring the distinction between 'Ident's
--   and 'Symbol's, i.e. @Ident "+:"@ amd @Symbol "+:"@ are equal.
cheatEq :: QName () -> QName () -> Bool
cheatEq (UnQual () (Symbol () s1)) (UnQual () (Ident  () s2)) = s1 == s2
cheatEq (UnQual () (Ident  () s1)) (UnQual () (Symbol () s2)) = s1 == s2
cheatEq q1                         q2                         = q1 == q2

-- | Gets the argument patterns of the given constructor pattern.
selectPats :: Pat () -> [Pat ()]
selectPats (PApp _ _ pats) = pats
selectPats (PInfixApp _ p1 _ p2) = [p1, p2]
selectPats p = error $ "selectPat: not definied for " ++ show p

-- | Gets the actual expression of the given right-hand side without guard.
selectExp :: Rhs () -> Exp ()
selectExp (UnGuardedRhs _ e) = e
selectExp _                  = error "selectExp: only unguarded rhs"

-- | Renames the corresponding pairs of variable patterns in the given
--   expression.
renameAll :: [(Pat (), Pat ())] -> Exp () -> PM (Exp ())
-- TODO refactor higher order foldr
-- TODO generate one Subst and apply only once
renameAll []               e = return e
renameAll ((from, to) : r) e = do
  f   <- getPVarName from
  t   <- getPVarName to
  res <- renameAll r e
  return $ rename (subst f t) res

-- | Applies 'optimizeAlt' to the given @case@ expression alternatives and
--   constructs a @case@ expression from the optimized alternatives.
--
--   While an alternative is optimized, the state contains a 'matchedPat'
--   entry for the current pair of scrutinee and pattern.
addAndOpt :: Exp () -> [Alt ()] -> PM (Exp ())
addAndOpt e alts = do
  alts' <- mapM (bindAndOpt e) alts
  return $ Case () e alts'
 where
  -- uses the list of Exp Pat as a stack
  bindAndOpt :: Exp () -> Alt () -> PM (Alt ())
  bindAndOpt v a@(Alt _ p _ _) = do
    stack <- gets matchedPat
    modify $ \state -> state { matchedPat = (v, p) : stack }
    alt' <- optimizeAlt a
    modify $ \state -> state { matchedPat = stack }
    return alt'

-- | Applies 'optimizeAlt' to all given @case@ expression alternatives.
optimizeAlts :: [Alt ()] -> PM [Alt ()]
optimizeAlts = mapM optimizeAlt

-- | Optimizes the right-hand side of the given @case@ expression alternative.
optimizeAlt :: Alt () -> PM (Alt ())
optimizeAlt (Alt _ p rhs _) = do
  let (UnGuardedRhs _ e) = rhs
  e' <- optimize e
  return $ Alt () p (UnGuardedRhs () e') B.noBinds
