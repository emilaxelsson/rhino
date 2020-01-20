module Main where

import Rhino.Prelude
import qualified Prelude

import Control.Exception (catch, throwIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (isSpace)
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import System.Exit (exitFailure, exitSuccess)

import qualified Data.Aeson as Aeson
import Options.Applicative

import Data.DAG
import Rhino.AST (Identifier (..), Input (..), ModulePath, Module)
import Rhino.Evaluate
import Rhino.InputProperties
import Rhino.Load
import Rhino.Reachability
import Rhino.StaticCheck

mainWrapper :: IO a -> IO a
mainWrapper m = do
  void $ m `catch` \(e :: SomeException) -> do
    Prelude.putStrLn $ displayException e
    exitFailure
  exitSuccess

data RhinoError
  = InputEnvFormatError String
  | MissingInputVariables [Identifier]
  deriving (Show)

instance Exception RhinoError where
  displayException (InputEnvFormatError aesonErr) = unlines
    ["Incorrectly formatted input environment; should be a JSON-formatted record with string keys."
    , ""
    , "Parse error:"
    , aesonErr
    ]
  displayException (MissingInputVariables vs) =
    "Missing variables in input environment:\n\n" ++
    unlines (map (toS . unIdentifier) vs)

data Options = Options
  { includes    :: [FilePath]
  , target      :: Maybe Identifier
  , listInputs  :: Bool
  , reachable   :: Bool
  , programFile :: FilePath
  } deriving (Eq, Show)

parseOptions :: Parser Options
parseOptions = Options
  <$> many
        ( strOption
          (  short 'i'
          <> long "include"
          <> metavar "INCLUDE_DIR"
          <> help "Directory to include when searching for modules"
          )
        )
  <*> optional
        ( strOption
          (  short 't'
          <> long "target"
          <> metavar "VARIABLE"
          <> help "Target variable (default: `main`)"
          )
        )
  <*> switch
        (  short 'l'
        <> long "list-inputs"
        <> help "List inputs"
        )
  <*> switch
        (  short 'r'
        <> long "reachable"
        <> help "List only reachable inputs"
        )
  <*> argument str
        (  metavar "FILE"
        <> help "Program file"
        )

-- | 'IO' is only used for throwing exceptions (convenient, since the exceptions
-- are of different type)
describeInputs ::
     Options
  -> DAG ModulePath (Annotated Module (StaticEnv, StaticEnv))
  -> IO (Map Identifier (Map Text Text))
describeInputs Options {..} modules = do
  inps <-
    fmap (fmap inputProps) $
    either throwIO return $ inputProperties $ unAnnnotateDAG modules
  return $ if reachable
    then let res = fromMaybe "main" target
          in inps `Map.restrictKeys` reachableInputs modules res
    else inps
  where
    inputProps Input {..} = Map.fromList $ catMaybes
      [ flip fmap inputType $ ("type", ) . unIdentifier
      , flip fmap inputLabel $ ("label", )
      ]

readInputEnv :: IO InputEnv
readInputEnv = do
  inp <- BS.getContents
  let menv = Aeson.eitherDecodeStrict inp
  case menv of
    Left aesonErr
      | Text.all isSpace (toS inp) -> return Map.empty
      | otherwise -> throwIO $ InputEnvFormatError aesonErr
    Right env -> return env

main :: IO ()
main = mainWrapper $ do
  opts@Options {..} <- execParser parseOptionsH
  let tgt = fromMaybe "main" target
  modules <- loadAndCheckProgamWithTarget includes programFile tgt
  if listInputs
    then LBS.putStr . Aeson.encode =<< describeInputs opts modules
    else do
      requiredInps <-
        describeInputs opts {target = Just tgt, reachable = True} modules
      env <- readInputEnv
      let missing = Map.keys $ Map.difference requiredInps env
      unless (null missing) $ throwIO $ MissingInputVariables missing
      LBS.putStr $ Aeson.encode $ evaluate env modules tgt
  where
    parseOptionsH = info (parseOptions <**> helper)
      (  fullDesc
      <> header "Interpreter for Rhino programs"
      )

-- About `LBS.putStr` and `BS.getContents`:
--
-- I'm not entirely sure how UTF-8 encoding works and whether there are problems
-- with using `putStrLn` from `bytestring`. But I note that this is what `aeson`
-- does in `decodeFile`/`encodeFile`, so it seems like the right thing to do.
