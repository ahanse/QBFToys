module DQBFParserSpec(spec) where

import DQBFParser
import DQBF
import Test.Hspec
import Text.Parsec(parse)

spec :: Spec
spec =
  describe "dqbfParser" $ do
    it "parses simple formula" $
      parse dqbfParser "" "A x1 : E {x1} y1 : x1 /\\ y1" `shouldBe` (Right (AQuant ["x1"], EQuant ["x1"] ["y1"], And (Var "x1") (Var "y1")))
