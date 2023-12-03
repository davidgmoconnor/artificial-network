module Config 
  ( Config (..)
  , parseArgs
  )
where

import Protolude
import Data.String ( String )

data Config = Config 
  { filePath :: String
  , premium  :: Int
  } deriving (Show)

parseArgs :: [String] -> Maybe Config 
parseArgs (f:i:_) = Config f <$> (readMaybe i)
parseArgs _ = Nothing