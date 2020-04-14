module Main
  ( main
  )
where

import           Control.Monad                  ( void )

import           Application
import           FreshVars
import           Language.Haskell.Exts
import           System.Console.GetOpt
import           System.Environment
import           System.FilePath

-- |The data type 'Options' contains all different flags one can set
-- for the executable. It also contains the FilePaths to the in and output
-- directories.
data Options = Options { showHelp     :: Bool
                       , inputFile    :: FilePath
                       , outputDir    :: Maybe FilePath
                       , enableDebug  :: Bool
                       , trivialCase  :: Bool
                       , optimizeCase :: Bool}

-- |The defaultOptions for the transformation applied if no flags are set.
defaultOptions :: Options
defaultOptions = Options { showHelp     = False
                         , inputFile    = error "Loading File failed"  -- ".\\Examples\\DebugInput.hs"
                         , outputDir    = Nothing    -- Just  ".\\Examples\\DebugOutput.hs"
                         , enableDebug  = False
                         , trivialCase  = False
                         , optimizeCase = True
                         }

-- | The function 'option' contains a list of OptDescr.
-- They contain manipulations for the options refering to one set flag each.
options :: [OptDescr (Options -> Options)]
options =
  [ Option ['h', '?']
           ["help"]
           (NoArg (\opts -> opts { showHelp = True }))
           "Show help"
  , Option ['d']
           ["debug"]
           (NoArg (\opts -> opts { enableDebug = True }))
           "Prints all Debug messages generated during the Phases"
  , Option ['t']
           ["trivialCC"]
           (NoArg (\opts -> opts { trivialCase = True }))
           "Enables trivial Case Completion"
  , Option ['n']
           ["noOptimization"]
           (NoArg (\opts -> opts { optimizeCase = False }))
           "Disables optimization for case expressions"
  , Option ['I']
           ["Input"]
           (ReqArg (\fp opts -> opts { inputFile = fp }) "DIR")
           "input DIR"
  , Option ['o']
           ["output"]
           (ReqArg (\m opts -> opts { outputDir = Just m }) "DIR")
           "output DIR"
  ]
-- |The function 'compilerOpts' takes a list of Strings and returns an option
-- and a list of non-options. In case of an error the function prints all errors
-- to the console.
compilerOpts :: [String] -> IO (Options, [String])
compilerOpts args = case getOpt Permute options args of
  (o, n, []  ) -> return (foldl (flip id) defaultOptions o, n)
  (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where header = "" -- TODO meaningfull header

-- | The function 'transformOptions' takes options and transforms them
-- into a PMState.
transformOptions :: Options -> PMState
transformOptions opts = PMState { nextId      = 0
                                , constrMap   = specialCons
                                , matchedPat  = []
                                , trivialCC   = trivialCase opts
                                , opt         = optimizeCase opts
                                , debugOutput = ""
                                }

-- | The 'main' function collects the command line arguments and transforms
-- them into a state to start the code transformation. Also handles the
-- files and parses the module.
main :: IO ()
main = do
  args           <- getArgs
  (opts, noOpts) <- compilerOpts args
  if not (showHelp opts)
    then do
      let state = transformOptions opts
      input <- readFile $ inputFile opts
      let x = fromParseResult (parseModule input)
          m = evalPM (processModule (void x)) state
      case outputDir opts of
        Just out -> do
          writeFile out (pPrint m)
          printDebug (enableDebug opts) state
        Nothing -> do
          putStr $ pPrint m
          printDebug (enableDebug opts) state
    else putStr (usageInfo "" options)

-- | The printDebug' function takes a bool and a PMState. If the bool is True
-- the debugOutput from the PMState is printed to the console.
printDebug :: Bool -> PMState -> IO ()
printDebug b s | b         = print $ "DebugOutput:" ++ debugOutput s
               | -- TODO pretty debug
                 otherwise = return ()

pPrint :: Module () -> String
pPrint = prettyPrintStyleMode
  (Style { mode = PageMode, lineLength = 120, ribbonsPerLine = 1.5 })
  defaultMode
