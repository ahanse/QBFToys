module DQBFParserSpec(spec) where

import DQBFParser
import DQBF
import Test.Hspec
import Text.Parsec(parse)


spec :: Spec
spec = let
  -- Trival test cases used to test parsing of other parts of the formula
  (ta, ta_p) = ("A x1:",[AQuant ["x1"]])
  (te, te_p) = ("E{x1} y1:",[EQuant ["x1"] ["y1"]])
  (tm, tm_p) = ("x1", Var "x1")

  -- Valid for all testcases
  simpForall = [
   "A x1 x2 : A x3 :",
   "Ax1 x2:Ax3:",
   "A x1\n x2 :A x3\n\r                 :     ",
   "\n\n       Ax1  \nx2:Ax3:"
   ]
  simpForallP = [AQuant ["x1", "x2"],AQuant ["x3"]]
  forallCases = [
    (ta, ta_p)] ++ (zip simpForall $ cycle [simpForallP])

  -- Valid exists testcases
  simpExists = [
    "E {x1 x2} y1 : E {x3} y2 :",
    "E{x1 x2}y1:E{x3}y2:",
    "\n\n    E    {x1 x2} y1 : E {x3} y2 :"]
  simpExistsP = [EQuant ["x1","x2"] ["y1"], EQuant ["x3"] ["y2"]]
  existsCases = [
    (te, te_p),
    ("E {} y1 : E {} y2 y3: E {}:",[EQuant [] ["y1"], EQuant [] ["y2","y3"], EQuant [] []])] ++ (zip simpExists $ cycle [simpExistsP])

  parserInstanceSpecs (l,r) = it ("parses: "++show l) $ parse dqbfParser "" l `shouldBe` (Right r)
  in
  describe "dqbfParser" $ do
    it "parses simple formula" $
      parse dqbfParser "" "A x1 : E {x1} y1 : x1 /\\ y1" `shouldBe` Right ([AQuant ["x1"]], [EQuant ["x1"] ["y1"]], And (Var "x1") (Var "y1"))
    mapM_ (parserInstanceSpecs.(\(l,r)->(l++te++tm,(r,te_p,tm_p)))) forallCases
    mapM_ (parserInstanceSpecs.(\(l,r)->(ta++l++tm,(ta_p,r,tm_p)))) existsCases
