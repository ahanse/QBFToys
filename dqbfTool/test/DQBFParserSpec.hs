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
  -- Valid boolean formulas
  formulaCases = [
   ("x1 <--> x2 ^ x3 --> x4 /\\ x5 \\/ ~x6 ",
    Iff (Var "x1") (Xor (Var "x2") (If (Var "x3") (And (Var "x4") (Or (Var "x5") (Not (Var "x6"))))))),
   ("~(((((x1 <--> x2) ^ x3) --> x4) /\\ x5) \\/ x6)",
    Not (Or (And (If (Xor (Iff (Var "x1") (Var "x2")) (Var "x3")) (Var "x4")) (Var "x5")) (Var "x6"))),
   ("(x1 | x2 | !x4) & (~x2 | ~y3) & (abc1)", And
     (And
      (Or (Or (Var "x1") (Var "x2")) (Not (Var "x4"))) (Or (Not (Var "x2")) (Not (Var "y3"))))
     (Var "abc1"))
   ]
  parserInstanceSpecs (l,r) = it ("parses: "++show l) $ parse dqbfParser "" l `shouldBe` (Right r)
  in
  describe "dqbfParser" $ do
    it "parses simple formula" $
      parse dqbfParser "" "A x1 : E {x1} y1 : x1 /\\ y1" `shouldBe` Right ([AQuant ["x1"]], [EQuant ["x1"] ["y1"]], And (Var "x1") (Var "y1"))
    mapM_ (parserInstanceSpecs.(\(l,r)->(l++te++tm,(r,te_p,tm_p)))) forallCases
    mapM_ (parserInstanceSpecs.(\(l,r)->(ta++l++tm,(ta_p,r,tm_p)))) existsCases
    mapM_ (parserInstanceSpecs.(\(l,r)->(ta++te++l,(ta_p,te_p,r)))) formulaCases
