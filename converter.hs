import Qdimacs 

main :: IO ()
main = do
    input <- getContents
    let prop = readProblem $ lines input 
    let qProp = quantifieUndeclaredVars prop
    let nProp = normalizeVars qProp
    print prop
    print qProp
    print nProp
