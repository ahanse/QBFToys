import System.IO
import System.Environment
import Text.Printf
import System.Console.GetOpt
import System.Exit
import Data.ByteString.Lazy.Builder (hPutBuilder)
import Text.Parsec(parse)

import DQBF2THF
import DQBFParser(dqbfParser)
import THFdqbf(buildThf)

data Flag = Invert | Output String
    deriving (Eq,Ord,Show)

flags = [Option ['i'] [] (NoArg Invert)
            (unlines ["Negates the generated THF problem. The conjecture is",
             "a theorem iff the DQBF formular is false."]),
         Option ['o'] [] (ReqArg Output "FILE")
            "Output FILE"]

header = unlines ["Usage: dqbfTool [-i] [-o FILE]\n",
         "Converts dependent quantified boolean formulas",
         "given as .dqbf files into THF problems."]

getOutfileName ∷ [Flag] → Maybe String
getOutfileName = foldl f Nothing
    where f (Just a) _ = Just a
          f _ (Output s) = Just s
          f _ _ = Nothing

main ∷ IO ()
main = do
    args ← getArgs
    case getOpt Permute flags args of
        (args,_,[]) → do
            input ← getContents
            let parsed = parse dqbfParser "STDIO" input
            case parsed of
              Right dqbf → do
                let output = (buildThf.(transform (Invert `elem` args))) dqbf
                case (getOutfileName args) of
                    Just path → do
                        handle ← openFile path WriteMode 
                        hSetBinaryMode handle True
                        hSetBuffering handle $ BlockBuffering Nothing
                        hPutBuilder handle output
                        hClose handle
                    Nothing → do
                        -- stdout!
                        hPutBuilder stdout output
        (_,_,errs) → do
            hPutStrLn stderr (concat errs ++ usageInfo header flags)
            exitWith (ExitFailure 1)
