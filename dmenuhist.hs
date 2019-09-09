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

sortIoByHistMap histMap
    = unlines
    . map snd
    . sortBy ((flip compare) `on` fst)
    . map (\ioLine -> let sortValue = Map.findWithDefault 0 ioLine histMap in (sortValue, ioLine))
    . lines

readFileIfExists filePath = do
    fileExists <- doesFileExist filePath
    if fileExists
        then readFile filePath
        else return ""

updateHistMap histMap [] = histMap
updateHistMap histMap selectedEntry = Map.insertWith (+) (init selectedEntry) 1 histMap

histContentsToMap
    = foldr
        (\histLine acc ->
            let (v, k) = spanAndRemoveFirst Data.Char.isAlphaNum histLine
                vi = read v :: Integer in
            Map.insert k vi acc) Map.empty
    . lines

histMapToContents
    = unlines
    . map (\(k, vi) -> show vi ++ " " ++ k)
    . Map.toList

main = do
    (histFilePath:(cmd:cmdArgs)) <- getArgs

    (Just cmdStdIn, Just cmdStdOut, _, _) <- createProcess (proc cmd cmdArgs){ std_in = CreatePipe, std_out = CreatePipe }

    histContents <- readFileIfExists histFilePath
    let histMap = histContentsToMap histContents

    ioContents <- getContents
    let sortedIoContent = sortIoByHistMap histMap ioContents

    hPutStr cmdStdIn sortedIoContent
    hClose cmdStdIn

    selectedEntry <- hGetContents cmdStdOut
    let updatedHistMap = updateHistMap histMap selectedEntry

    writeFile histFilePath $ histMapToContents updatedHistMap
    putStr selectedEntry

