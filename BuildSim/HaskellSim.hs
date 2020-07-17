{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

import qualified Control.Monad
import qualified Data.Array.MArray
import qualified Data.Array.IO
import qualified System.IO
import qualified System.Exit
import qualified Data.List
import Simulator.All

import qualified Data.HashMap as M
import qualified Data.Vector as V
import qualified Data.BitVector as BV
import qualified Data.Vector.Mutable as MV

import Data.String
import Data.List
import Data.Maybe (isJust, catMaybes)
import Control.Monad
import Data.IORef
import System.Exit
import System.IO
import System.Random (randomIO)
import System.Environment (getArgs)
import Text.Read
import Control.Exception
import Data.BitVector as BV
import Data.Array.MArray as MA
import Data.Array.IO
import qualified Data.Map.Strict as Map

import HaskellTarget as T

chunksOf :: Int -> [a] -> [[a]]
chunksOf n [] = []
chunksOf n ys =
  let (val, rest) = splitAt n ys in
  (val : chunksOf n rest)

timeout :: Int
timeout = 20

kami_model :: ([RegFileBase] , BaseModule)
kami_model = snd (T.separateModRemove T.mainMod)

kami_hides :: [String]
kami_hides = fst (T.separateModRemove T.mainMod)

regfiles :: [RegFileBase]
regfiles = fst $ kami_model

basemod :: BaseModule
basemod = snd $ kami_model

data Environment = Environment {
  steps       :: Int,
  counter     :: Int
}

mkEnv :: IO Environment
mkEnv = return $ Environment 0 0

hasArg :: String -> IO Bool
hasArg name = getArgs >>= (.) return (maybe False (const True) . find (\arg -> (isPrefixOf name arg)))

io_stuff :: forall m a v. (StringMap m, Array a, Vec v) => FileState m a v -> m (Val v) -> Environment -> IO Environment
io_stuff filestate regstate env =
  let currSteps = steps env :: Int in do
    modes <- get_modes
    let interactive = interactive_mode modes
    if interactive && currSteps == 0
      then do 
        putStr "% "
        hFlush stdout
        input <- getLine
        case words input of
            ["Step",num] -> case readMaybe num of
                Nothing -> do
                    putStrLn "Formatting error."
                    io_stuff filestate regstate env
                Just n -> return $ env {steps = n}
            [reg] -> do 
                print_reg regstate $ reg
                io_stuff filestate regstate env
            [] -> io_stuff filestate regstate env
            _ -> do
                putStrLn "Formatting error."
                io_stuff filestate regstate env
      else return env

instance AbstractEnvironment Environment where
  envPost env filestate regstate ruleName = return env
  envPre env filestate regstate ruleName = 
    let currCounter = counter env
        currSteps   = steps env in
    do
      putStrLn $ "[main] rule name: " ++ ruleName
      putStrLn $ "[sim] current cycle count: " ++ show currCounter
      when (currCounter > timeout) $ do
        hPutStrLn stderr "TIMEDOUT"
        exitFailure
      io_stuff filestate regstate
        env {
          counter = (currCounter + 1),
          steps = if currSteps > 0
                    then currSteps - 1
                    else currSteps}

resetMeth :: StringMap m => Environment -> Val v -> FileState m a v -> m (Val v) -> IO (Environment, Val v)
resetMeth env v filestate regstate = return (env, BoolVal False)

getInputMeth :: StringMap m => Environment -> Val v -> FileState m a v -> m (Val v) -> IO (Environment, Val v)
getInputMeth env v filestate regstate = return (env, StructVal [("valid", BoolVal True), ("data", BVVal (bitVec 1 1))])

sendOutputMeth :: StringMap m => Environment -> Val v -> FileState m a v -> m (Val v) -> IO (Environment, Val v)
sendOutputMeth env v filestate regstate = return (env, BVVal BV.nil)

externalInterrupt :: StringMap m => Environment -> Val v -> FileState m a v -> m (Val v) -> IO (Environment, Val v)
externalInterrupt env _ _ _ = return (env, BoolVal False)

debugInterrupt :: StringMap m => Environment -> Val v -> FileState m a v -> m (Val v) -> IO (Environment, Val v)
debugInterrupt env _ _ _ = return (env, BoolVal False)

meths :: StringMap m => [(String, Environment -> Val v -> FileState m a v -> m (Val v) -> IO (Environment, Val v))]
meths = [
    ("reset",      resetMeth),
    ("getInput",   getInputMeth),
    ("sendOutput", sendOutputMeth)
  ]

poly_main :: forall m a v. (StringMap m, Array a, Vec v) => IO()
poly_main = do
  hSetBuffering stdout NoBuffering
  env <- mkEnv
  envRef <- newIORef env
  hPutStrLn stdout "[main] starting the simulation"
  simulate_module @m @a @v 0 round_robin_rules envRef (map fst $ getRules basemod) Main.meths regfiles kami_hides basemod
  return ()

main :: IO ()
main = poly_main @(Map.Map String) @(MV.IOVector) @V.Vector
