import Qdimacs 
import Thf
import System.IO
import System.Environment
import Text.Printf
import System.Console.GetOpt
import System.Exit

data Flag = Normalize | Output String
    deriving (Eq,Ord,Show)


flags = [Option ['n'] [] (NoArg Normalize)
            "Outputs a standard complient Qdimacs problem.",
         Option ['o'] [] (ReqArg Output "FILE")
            "Output FILE"]

header = unlines ["Usage: convert [-n] [-o FILE]\n",
         "If an output filename is given a line containing the",
         "number of variables, clauses and if the problem was",
         "\"Trivally True\" (contained no clause) or \"Trivally False\"",
         "(contained the empty clause) is written to stdout."]

getOutfileName :: [Flag] -> Maybe String
getOutfileName = foldl f Nothing 
    where f (Just a) _ = Just a
          f _ (Output s) = Just s
          f _ _ = Nothing
    
cleanQBF :: QBFProblem -> (String, QBFProblem)
cleanQBF p = (s,p'')
    where 
        p' = (normalizeVars.quantifieUndeclaredVars) p
        (c, p'') = isTrivial p'
        (nv, nc) = qbfSizeNaive p''
        s = printf "%d %d %s" nv nc (case c of
                Nothing -> ""
                Just True -> "Trivally True"
                Just False -> "Trivally False")

main :: IO ()
main = do
    args <- getArgs
    case getOpt Permute flags args of
        (args,_,[]) -> do
            input <- getContents
            let prop = readProblem $ lines input 
                (desc, prop') = cleanQBF prop
                output = (if Normalize `elem` args 
                            then toQdimacs else toThf) prop'
                maybePath = getOutfileName args
            case maybePath of
                Just path -> do 
                    handel <- openFile path WriteMode
                    mapM_ (hPutStrLn handel) output 
                    hClose handel
                    putStrLn desc
                Nothing -> do 
                    mapM_ putStrLn output
        (_,_,errs) -> do
            hPutStrLn stderr (concat errs ++ usageInfo header flags)
            exitWith (ExitFailure 1)

