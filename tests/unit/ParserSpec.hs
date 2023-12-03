module ParserSpec where 

import Protolude
import Test.Hspec  
import Core.Network

import Text.Megaparsec ( runParser, errorBundlePretty )

spec :: Spec 
spec = do 
  describe "Parsing networks" $ do 
    it "can parse single line" $ do 
      let input = "I1: 100%"
      let network = [Line 
                      { insurer = InsurerID 1
                      , percentage = Percentage 100
                      , expressions = []
                      }]
      let result = runParser pNetwork "" input
      result `shouldBe` Right network
    it "can parse single line with expressions" $ do 
      let input = "I1: 100% if I2 and <3 and >4 and =5"
      let network = [Line 
                      { insurer = InsurerID 1
                      , percentage = Percentage 100
                      , expressions = [Insurer 2, LessThan 3, MoreThan 4, EqualTo 5]
                      }]
      let result = runParser pNetwork "" input
      result `shouldBe` Right network
    it "can parse multiple lines with expressions" $ do 
      let input = "I1: 100% if I2 and <3\nI2: 100% if >3\nI3: 100%\nI4: 19%"
      let network = [Line 
                      { insurer = InsurerID 1
                      , percentage = Percentage 100
                      , expressions = [Insurer 2, LessThan 3]
                      },
                    Line 
                      { insurer = InsurerID 2
                      , percentage = Percentage 100
                      , expressions = [MoreThan 3]
                      },
                    Line 
                      { insurer = InsurerID 3
                      , percentage = Percentage 100
                      , expressions = []
                      },
                    Line 
                      { insurer = InsurerID 4
                      , percentage = Percentage 19
                      , expressions = []
                      }]
      let result = runParser pNetwork "" input
      result `shouldBe` Right network
    it "can handle syntax errors in insurer" $ do 
      let input = "Y1: 100%"
      let result = runParser pNetwork "" input
      case result of
        Left err -> errorBundlePretty err `shouldBe` "1:1:\n  |\n1 | Y1: 100%\n  | ^\nunexpected 'Y'\nexpecting 'I'\n"
        Right _ -> expectationFailure "Should have failed"
    it "can handle syntax errors in percentage" $ do 
      let input = "I1: 0.1%"
      let result = runParser pNetwork "" input
      case result of
        Left err -> errorBundlePretty err `shouldBe` "1:6:\n  |\n1 | I1: 0.1%\n  |      ^\nunexpected '.'\nexpecting '%' or digit\n"
        Right _ -> expectationFailure "Should have failed"