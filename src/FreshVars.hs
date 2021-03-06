module FreshVars (module FreshVars
                  , gets
                  , modify
                  , runState
                  , evalState) where                                                   -- import Control.MonadState

import           Control.Monad.State
import qualified Language.Haskell.Exts.Syntax as S

type Constructor = (S.QName (),Int,Bool) -- QName instead of String to support special Syntax
                                         -- Bool isInfix

getConstrArity :: Constructor -> Int
getConstrArity (_,a,_) = a

getConstrName :: Constructor -> S.QName ()
getConstrName (n,_,_)= n

isInfixConst :: Constructor -> Bool
isInfixConst (_,_,b) = b

data PMState = PMState { nextId      :: Int
                       , constrMap   :: [(String, [Constructor])] -- Arity
                       , matchedPat  :: [(S.Exp (), S.Pat () )]   -- Variable and binded Cons
                       , trivialCC   :: Bool
                       , opt         :: Bool                      -- optimize case exps
                       , debugOutput :: String}

type PM a = State PMState a

freshVar :: PM Int
freshVar = do
  i <- gets nextId
  modify $ \state -> state { nextId = i + 1 }
  --debug <- gets debugOutput
  --modify $ \state -> state {debugOutput = "Generated"++ show i ++", "++debug}
  return i

addConstrMap :: (String, [Constructor]) -> PM ()
addConstrMap cs = do
  cmap <- gets constrMap
  modify $ \state -> state {constrMap = (cs:cmap)}
    {- renameFunc :: FuncDecl -> FreshVar FuncDecl
    renameFunc (Func ...) = do
      i <- freshVar
      return (FuncDecl ...) -}

            {- processProg :: Prog -> Prog
            processProg p = evalState (renameProg p) 0 -}

addDebug :: String -> PM ()
addDebug s = do
  debug <- gets debugOutput
  modify $ \state -> state {debugOutput = (s ++ debug)}
