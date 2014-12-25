import Qdimacs 

main :: IO ()
main = do
    input <- getContents
    let norm = toText.normalizeVars.quantifieUndeclaredVars
    let prop = readProblem $ lines input 
    mapM_ putStrLn $ norm prop
