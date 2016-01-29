{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Exception
import           Control.Monad

import           Data.Yaml hiding (Parser)
import           Data.ByteString      (ByteString)
import qualified Data.ByteString      as B
import qualified Data.HashMap.Strict  as H
import qualified Data.Text            as T
import           Options.Applicative

import           System.Directory     (getHomeDirectory)
import           System.FilePath.Find
import "Glob"    System.FilePath.Glob
import           System.IO.Error
import           System.Process.ByteString

import Debug.Trace


data Config = Config
  { globalExcludes :: [Pattern]
  , executables :: [Executable]
  } deriving Show

instance FromJSON Pattern where
  parseJSON (String pat) = return $ compile $ T.unpack pat
  parseJSON _          = mzero

instance FromJSON Config where
  parseJSON (Object v) = Config
                         <$> v .:? "excludes" .!= []
                         <*> v .: "executables"
  parseJSON _          = mzero

instance FromJSON Executable where
  parseJSON (Object v) = case H.toList v of
                           (exe, Object d):[] -> do
                                       flags <- d .:? "flags" .!= []
                                       exts  <- d .: "extensions"
                                       return $ Executable (T.unpack exe) flags exts
                           _ -> mzero
  parseJSON _          = mzero

-- TODO: error handling
readConfig :: FilePath -> IO (Maybe Config)
readConfig file = catch (decodeFile file)
                        (\e -> putStrLn (prettyPrintParseException (e :: ParseException))
                                         >> return Nothing)

data Options = Options
  { append  :: Bool
  , tagfile :: FilePath
  , recurse :: Bool
  , sort    :: Bool
  , verbose :: Bool
  , exclude :: [Pattern]
  , files   :: [FilePath]
  , config  :: FilePath
  } deriving Show

opts :: FilePath -> Parser Options
opts home = Options
  <$> switch
      ( long "append"
     <> help "Append to existing tag file. Otherwise tag file will be overwritten"
     <> short 'a'
      )
  <*> strOption
      ( long "tagfile"
     <> short 'f'
     <> short 'o'
     <> help "Tagfile to output to"
     <> value "tags"
     <> metavar "FILE"
     <> showDefault
      )
  <*> switch
      ( long "recurse"
     <> short 'R'
     <> help "Recursively find files"
      )
  <*> switch
      ( long "sort"
     <> short 'u'
     <> help "Sort tag files"
      )
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Verbose"
      )
  <*> many (compile <$> strOption (
              long "exclude"
           <> help "pattern to exclude"
           <> metavar "PATTERN"
            ))
  <*> some (strArgument (
              metavar "FILES"
           ))
  <*> strOption
      ( long "config"
     <> help "Specify config file"
     <> metavar "FILE"
     <> value (home <> "/.ctags-shim.yaml")
     <> showDefault
      )

type Extension = String
data Executable = Executable
  { binary :: String
  , flags  :: [String]
  , extensions :: [Extension]
  } deriving Show

-- default executables (just ctags for now)
defaultExecutables :: [Executable]
defaultExecutables  = [ Executable "ctags" ["-f -"] ["c", "cpp", "h", "hpp", "py"]
                      ]

-- TODO: dont call this file multiple times. Recurse once and filter by extension after
findFiles :: Bool -> [FilePath] -> [Pattern] -> [Extension] -> IO [FilePath]
findFiles recurse paths pats exs = mapM findFiltered paths
                                   >>= return . concat
  where
    findFiltered = find (excludes &&? return recurse) (extensions &&? excludes)
    extensions = foldr (\e b -> b ||? extension ==? '.':e) (return False) exs
    excludes = foldr (\pat b -> b ||? globMatch pat filePath) (return False) pats
                 ==? False -- want to make sure no excludes are true
    globMatch pat = liftM (match pat)

-- TODO: error handling
-- TODO: batching?
tagsExec :: Executable -> [FilePath] -> IO ByteString
tagsExec (Executable exe flags extensions) files = B.concat <$> mapM execOne files
  where
    execOne file = do
      (code, stdout, stderr) <- readProcessWithExitCode exe (flags <> [file]) B.empty
      return stdout

-- TODO: check that tags file is valid
-- TODO: update only relevant parts
-- TODO: lock file
-- TODO: sorting
-- TODO: verbose
-- TODO: pay attention to .gitignore
-- TODO: merge default executables with conf
-- TODO: look for local config
main :: IO ()
main = do
  home <- getHomeDirectory
  opts <- execParser (info (helper <*> opts home) fullDesc)
  conf <- readConfig (config opts)
  let gexcludes = maybe [] globalExcludes conf
  let excludes = (exclude opts) <> gexcludes
  let execs = maybe defaultExecutables executables conf

  found <- mapM (findFiles (recurse opts) (files opts) excludes) (map extensions execs)
  tags <- zipWithM tagsExec execs found
  (if append opts then B.writeFile else B.appendFile) (tagfile opts) (B.concat tags)
  return ()