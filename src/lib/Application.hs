module Application
  ( processModule
  , specialCons
  , useAlgoModule
  )
where                                      -- TODO too many variables generated
                                                                                -- TODO only tuples supported
import           Algo
import qualified CaseCompletion                as CC
import           FreshVars
import qualified GuardElimination              as GE
import           Language.Haskell.Exts.Syntax

-- |The function 'useAlgo' applies the algorithm on each declaration in the module.
useAlgoModule :: Module () -> PM (Module ())
useAlgoModule (Module _ mmh mps ids ds) = do
  dcls <- mapM useAlgoDecl ds
  return $ Module () mmh mps ids dcls
useAlgoModule _ = error "useAlgoModule: not on module"

-- |The function 'useAlgoDecl' applies the algorithm on the the FunBinds
useAlgoDecl :: Decl () -> PM (Decl ())
useAlgoDecl (FunBind _ ms) = do
  nms <- useAlgoMatches ms
  return (FunBind () nms)
useAlgoDecl v = return v

-- TODO maybe refactor to fun decl or check if oneFun stuff is needed or always true
useAlgoMatches :: [Match ()] -> PM ([Match ()])
useAlgoMatches []       = return []
useAlgoMatches (m : ms) = do
  let (oneFun, r) = span (GE.comp m) ms
  if (length oneFun) >= 1 || hasCons (m : oneFun)
    then do
      x  <- useAlgo (m : oneFun)
      xs <- useAlgoMatches r
      return (x : xs)
    else do
      xs <- useAlgoMatches r
      return (m : xs)

-- |Checks a given list of Matches for constructor pattern.
-- Returns True if the list contains more than one Match or if any of the pattern
-- is a constructor pattern.
hasCons :: [Match ()] -> Bool
hasCons [m] = case m of
  Match _ _ ps _ _         -> any isCons ps
  InfixMatch _ p1 _ ps _ _ -> any isCons (p1 : ps)
hasCons _ = True -- False?

-- |The function 'useAlgo' applies the match function to a list of matches
-- returning a single Match.
useAlgo
  :: [Match ()]    -- all matches for one function name
  -> PM (Match ()) -- contains one match
useAlgo ms = do
  let mname    = GE.getMatchName ms
  let eqs = map (\(Match _ _ pats rhs _) -> (pats, selectExp rhs)) ms
  let funArity = (length . fst . head) eqs
  nVars <- newVars funArity
  nExp  <- match nVars eqs err
  b     <- gets opt
  if b
    then do
      oExp <- optimize nExp
      return $ Match () mname nVars (UnGuardedRhs () oExp) Nothing
    else return $ Match () mname nVars (UnGuardedRhs () nExp) Nothing
 where
  selectExp :: Rhs () -> Exp ()
  selectExp (UnGuardedRhs _ e) = e
  selectExp _                  = error "no UnGuardedRhs in selectExp"

-- a general version of add
addG :: (a -> PM ()) -> Maybe a -> PM ()
addG = maybe (return ())
-- addG f ma = maybe (return()) f ma

-- | The function 'collectDataInfo' takes a module and writes all datatype
-- declarations into the State with their name and constructors.
collectDataInfo :: Module () -> PM ()
collectDataInfo (Module _ _ _ _ decls) = do
  mas <- sequence (map collectDataDecl decls)
  mapM_ (addG addConstrMap) mas
  return ()
collectDataInfo _ = return ()

-- |The function 'collectDataDecl' takes a Declaration and returns a pair of
-- a datatype name and a list of cunstructors if the declaration was a DataDecl.
-- Returns Nothing otherwise.
collectDataDecl :: Decl () -> PM (Maybe (String, [Constructor]))
collectDataDecl (DataDecl _ (DataType _) _ dhead qcdecls _) =
  return $ Just ((getDataName dhead), (map getDataCons qcdecls))
collectDataDecl _ = return Nothing

-- |The function 'getDataName' takes a DeclHead and returns a string with the
-- name of the data type.
getDataName :: DeclHead () -> String                                            -- add symbols?
getDataName (DHead _ dname ) = fromName dname
getDataName (DHApp _ decl _) = getDataName decl
getDataName (DHParen _ decl) = getDataName decl
getDataName _ = error "getDataName: Symbol or infix in declaration" --TODO Test symbols and infix

-- |The function 'getDataName' takes a QualConDecl and returns the contained constructor.
getDataCons :: QualConDecl () -> Constructor
getDataCons (QualConDecl _ _ _ cdecl) = getDataCons' cdecl
 where
  getDataCons' :: ConDecl () -> Constructor
  getDataCons' (ConDecl _ cname types) = (UnQual () cname, length types, False)
  getDataCons' (InfixConDecl _ _ cname _) = (UnQual () cname, 2, True)
  getDataCons' (RecDecl _ _ _) = error "record notation is not supported"

-- |The function 'fromName' takes a Name and returns its String.
fromName :: Name () -> String
fromName (Ident  _ str) = str
fromName (Symbol _ str) = str

-- |The function 'processModule' sequentially applies the different transformations
-- to the given module after collecting the data types. Returns a new module with
-- the transformed functions.
processModule :: Module () -> PM (Module ())
processModule m = do
  collectDataInfo m                                                          -- TODO  maybe unused
  eliminatedM    <- GE.applyGEModule m
  caseCompletedM <- CC.applyCCModule eliminatedM
  nm             <- useAlgoModule caseCompletedM
  return nm

-- | 'specialCons' is a map for the sugared data types in Haskell, since they
-- can not be defined in a module by hand.
-- This map is the default 'constrMap' for the PMState used in Main.hs
specialCons :: [(String, [Constructor])]
specialCons =
  [ ("unit", [(Special () (UnitCon ()), 0, False)])
  , ( "list"
    , [(Special () (ListCon ()), 0, False), ((Special () (Cons ())), 2, True)]
    )
  , ("fun", [(Special () (FunCon ()), 2, True)])
  , ( "pair"
    , [(Special () (TupleCon () Boxed 2), 2, False)]
    )    -- TODO Tuples
  , ("wildcard", [(Special () (ExprHole ()), 0, False)])
  ]
