module Main (main) where

import Protolude
import System.IO        hiding ( print )
import Data.String             ( String )
import Data.Text               ( pack )
import Text.Megaparsec         ( errorBundlePretty
                               , runParser
                               )
import Config                  ( Config (..)
                               , parseArgs
                               )
import Core.Network            ( Network
                               , validateNetwork 
                               , pNetwork
                               )
import Core.Solver             ( solve )

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of 
    Nothing -> do
      print ("Invalid args"::String)
    Just Config {..} -> do
      network <- withFile filePath ReadMode getNetwork
      case network of 
        Left err -> print err
        Right n -> do
          let valid = validateNetwork n
          case valid of 
            Left validationErr -> print validationErr
            Right _ -> print $ solve premium n
          

getNetwork :: Handle -> IO (Either Text Network)
getNetwork h = do
  content <- hGetContents h
  let parsedNetwork = case runParser pNetwork "" (pack content) of
                        Left err  -> Left . pack $ errorBundlePretty err
                        Right res -> Right res
  return $!! parsedNetwork

