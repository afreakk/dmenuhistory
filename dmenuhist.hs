import Data.Char
import System.IO
import System.Process
import qualified Data.Map as Map
import Data.List (sortBy)
import Data.Function (on)
import System.Environment
import System.Directory

spanAndRemoveFirst::(a -> Bool) -> [a] -> ([a],[a])
spanAndRemoveFirst p [] = ([],[])
spanAndRemoveFirst p xs@(x:xs') 
            | p x       =  (x:ys,zs) 
            | otherwise =  ([], drop 1 xs)
                           where (ys,zs) = spanAndRemoveFirst p xs'

applyHistory histMap ioLine = (sortValue, ioLine)
    where sortValue = Map.findWithDefault 0 ioLine histMap

getSorted histMap
    = (map snd)
    . (sortBy ((flip compare) `on` fst))
    . map (applyHistory histMap)

histLineToMap histLine acc =
    Map.insert k vi acc
        where (v, k) = spanAndRemoveFirst Data.Char.isAlphaNum histLine
              vi = read v :: Integer

readFileIfExists filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then readFile filePath
        else return ""

updateHistMap histMap [] = histMap
updateHistMap histMap selectedEntry = Map.insertWith (+) (init selectedEntry) 1 histMap

main = do
    (histFilePath:(cmd:args)) <- getArgs
    histContents <- readFileIfExists histFilePath
    let histLines = lines histContents
    let histMap = foldr histLineToMap Map.empty histLines
    ioContents <- getContents
    let sortedLines = getSorted histMap $ lines ioContents

    (Just fzfStdIn, Just fzfStdOut, _, _) <- createProcess (proc cmd args){ std_in = CreatePipe, std_out = CreatePipe }
    hPutStr fzfStdIn $ unlines sortedLines
    hClose fzfStdIn
    selectedEntry <- hGetContents fzfStdOut
    let updatedHistMap = updateHistMap histMap selectedEntry
    writeFile histFilePath $ unlines $ map (\(k,v) -> (show v) ++ " " ++ k) $ Map.toList updatedHistMap
    putStr selectedEntry

