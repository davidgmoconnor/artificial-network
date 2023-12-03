module Core.Network 
  ( Network
  , Line (..)
  , InsurerID (..)
  , Percentage (..)
  , Expression (..)
  , sum
  , validateNetwork 
  , pNetwork
  , parseNetwork
  )
where

import Protolude hiding (sum, replace, isInfixOf, some, try)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Text ( splitOn, replace, strip, isInfixOf )

-- Core Types
-------------------------------------------------------------------------------

data Expression 
  = LessThan Int
  | MoreThan Int
  | EqualTo Int
  | Insurer Int
  deriving (Show, Generic, Eq)

newtype InsurerID = InsurerID { unInsurerID :: Int } deriving (Show, Generic, Eq) 
newtype Percentage = Percentage { unPercentage :: Int } deriving (Show, Generic, Eq) 

data Line = Line
  { insurer :: InsurerID
  , percentage :: Percentage
  , expressions :: [Expression]
  } deriving (Show, Generic, Eq)

instance NFData InsurerID
instance NFData Percentage
instance NFData Expression
instance NFData Line

type Network = [Line]

-- Utils
-------------------------------------------------------------------------------

sum :: Percentage -> Percentage -> Percentage
sum (Percentage a) (Percentage b) = Percentage $ a + b

validateNetwork :: Network -> Either Text ()
validateNetwork n = 
  if (foldr (||) False $ (map (>100)) $ map (unPercentage . percentage) n)
    then Left "Percentages over 100 not allowed"
    else Right ()

-- MegaParsec Parser
-------------------------------------------------------------------------------

type Parser = Parsec Void Text

pNetwork :: Parser Network 
pNetwork = pLine `sepEndBy1` newline

pLine :: Parser Line 
pLine = do 
  insurer <- pInsurerID
  void (string ": ")
  percentage <- pPercentage
  void (optional $ char ' ')
  me <- optional $ pExpressions
  let expressions = fromMaybe [] me
  return Line {..}
    
pExpressions :: Parser [Expression]
pExpressions = do 
  string "if " *> pExpression `sepEndBy1` try (string " and ")

pExpression :: Parser Expression 
pExpression = choice [
    pLessThan,
    pMoreThan,
    pEqualTo,
    pInsurer
  ]

pLessThan :: Parser Expression 
pLessThan = do 
  void (char '<')
  LessThan <$> L.decimal

pMoreThan :: Parser Expression 
pMoreThan = do 
  void (char '>')
  MoreThan <$> L.decimal

pEqualTo :: Parser Expression 
pEqualTo = do 
  void (char '=')
  EqualTo <$> L.decimal

pInsurer :: Parser Expression 
pInsurer = do 
  void (char 'I')
  Insurer <$> L.decimal

pPercentage :: Parser Percentage 
pPercentage = do 
  p <- Percentage <$> L.decimal 
  void (char '%')
  return p
  
pInsurerID :: Parser InsurerID
pInsurerID = do
  void (char 'I')
  InsurerID <$> L.decimal

-- Manual Parser
-------------------------------------------------------------------------------

parseNetwork :: [Text] -> Either Text Network
parseNetwork = traverse parseLine

parseLine :: Text -> Either Text Line
parseLine t = do
  let insurerId = getInsurerId t
  let percentage = getPercentage t
  let exp = getExpressions t
  Line <$> insurerId <*> percentage <*> exp

getInsurerId :: Text -> Either Text InsurerID
getInsurerId t = 
  case (splitOn ":" t) of 
    (x:_) -> toInt "I" "InsurerID" InsurerID x
    _ -> Left "Invalid Insurer ID"

getPercentage :: Text -> Either Text Percentage
getPercentage t = 
  case (splitOn ":" t) of
    (_:xs) -> do
      case splitOn "%" (unwords xs) of 
        (y:_) -> toInt "%" "Percentage" Percentage y
        _     -> Left "Invalid Percentage"
    _ -> Left "Invalid Percentage"

getExpressions :: Text -> Either Text [Expression]
getExpressions t = 
  case (splitOn "if" t) of 
    (_:xs) -> do
      let expressions = filter (/= "") $ map strip $ splitOn "and" $ unwords xs 
      traverse matchExpression expressions
    _      -> Right []

matchExpression :: Text -> Either Text Expression 
matchExpression ex 
  | isInfixOf "<" ex = toInt "<" "Less Than" LessThan ex
  | isInfixOf ">" ex = toInt ">" "More Than" MoreThan ex
  | isInfixOf "=" ex = toInt "=" "Equal To" EqualTo ex
  | isInfixOf "I" ex = toInt "I" "Insurer" Insurer ex
  | otherwise        = Left $ "Invalid Expression" <> ex

toInt :: Text -> Text -> (Int -> a) -> Text -> Either Text a
toInt prefix name constructor ex = 
  case readMaybe (replace prefix "" ex) of 
    Nothing -> Left $ "Invalid " <> name <> " in Expression"
    Just v -> Right . constructor $ v