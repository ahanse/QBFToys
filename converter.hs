import Qdimacs 
import Thf
import System.IO
import System.Environment
import Text.Printf

getOutfileName :: IO (Maybe String)
getOutfileName = do
    args <- getArgs
    case args of 
        ("-o":name:_) -> return (Just name)
        _ -> return Nothing

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
    input <- getContents
    let prop = readProblem $ lines input 
    let (desc, prop') = cleanQBF prop
    let output = toThf prop'
    maybePath <- getOutfileName
    case maybePath of
        Just path -> do 
            handel <- openFile path WriteMode
            mapM_ (hPutStrLn handel) output 
            hClose handel
            putStrLn desc
        Nothing -> do 
            mapM_ putStrLn output
