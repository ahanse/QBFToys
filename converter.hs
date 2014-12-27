import Qdimacs 

main :: IO ()
main = do
    input <- getContents
    let norm = toQdimacs.normalizeVars.quantifieUndeclaredVars
    let prop = readProblem $ lines input 
    mapM_ putStrLn $ norm prop
