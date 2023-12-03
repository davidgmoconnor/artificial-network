module SolverSpec where 

import Protolude
import Test.Hspec 
import Core.Network 
import Core.Solver ( solve )

spec :: Spec 
spec = do 
  describe "Solving for minimum insurers" $ do 
    it "can attribute to single insurer" $ do 
      let network = [Line 
                      { insurer = InsurerID 1
                      , percentage = Percentage 100
                      , expressions = []
                      }]
      let result = solve 1 network 
      result `shouldBe` [1]
    it "can split between two insurers" $ do 
      let network = [Line 
                      { insurer = InsurerID 1
                      , percentage = Percentage 50
                      , expressions = []
                      },
                    Line 
                      { insurer = InsurerID 2
                      , percentage = Percentage 50
                      , expressions = []
                      }]
      let result = solve 1 network 
      result `shouldBe` [1, 2]
    it "finds no solution if sum to less than 100" $ do 
      let network = [Line 
                      { insurer = InsurerID 1
                      , percentage = Percentage 50
                      , expressions = []
                      },
                    Line 
                      { insurer = InsurerID 2
                      , percentage = Percentage 40
                      , expressions = []
                      }]
      let result = solve 1 network 
      result `shouldBe` []
    it "finds no solution if sum to more than 100" $ do 
      let network = [Line 
                      { insurer = InsurerID 1
                      , percentage = Percentage 90
                      , expressions = []
                      },
                    Line 
                      { insurer = InsurerID 2
                      , percentage = Percentage 40
                      , expressions = []
                      }]
      let result = solve 1 network 
      result `shouldBe` []
    it "can filter insurers with less than expression" $ do 
      let network = [Line 
                      { insurer = InsurerID 1
                      , percentage = Percentage 100
                      , expressions = [LessThan 4]
                      },
                    Line 
                      { insurer = InsurerID 2
                      , percentage = Percentage 100
                      , expressions = []
                      }]
      let result = solve 5 network 
      result `shouldBe` [2]
    it "can filter insurers with more than expression" $ do 
      let network = [Line 
                      { insurer = InsurerID 1
                      , percentage = Percentage 100
                      , expressions = [MoreThan 4]
                      },
                    Line 
                      { insurer = InsurerID 2
                      , percentage = Percentage 100
                      , expressions = []
                      }]
      let result = solve 3 network 
      result `shouldBe` [2]
    it "can filter insurers with equal to expression" $ do 
      let network = [Line 
                      { insurer = InsurerID 1
                      , percentage = Percentage 90
                      , expressions = []
                      },
                    Line 
                      { insurer = InsurerID 2
                      , percentage = Percentage 100
                      , expressions = [EqualTo 3]
                      }]
      let result = solve 3 network 
      result `shouldBe` [2]
    it "can filter insurers with insurer expression" $ do 
      let network = [Line 
                      { insurer = InsurerID 1
                      , percentage = Percentage 50
                      , expressions = []
                      },
                    Line 
                      { insurer = InsurerID 2
                      , percentage = Percentage 50
                      , expressions = [Insurer 4]
                      },
                    Line 
                      { insurer = InsurerID 3
                      , percentage = Percentage 50
                      , expressions = []
                      }]
      let result = solve 3 network 
      result `shouldBe` [1,3]
    it "returns minimal solutions" $ do 
      let network = [Line 
                      { insurer = InsurerID 1
                      , percentage = Percentage 100
                      , expressions = []
                      },
                    Line 
                      { insurer = InsurerID 2
                      , percentage = Percentage 50
                      , expressions = []
                      },
                    Line 
                      { insurer = InsurerID 2
                      , percentage = Percentage 50
                      , expressions = []
                      }]
      let result = solve 3 network 
      result `shouldBe` [1]