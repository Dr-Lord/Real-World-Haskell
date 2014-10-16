-- File: Real World Haskell\Ch8\Useful.hs
import System.FilePath (replaceExtension)
import System.Directory (doesFileExist, renameDirectory, renameFile, getCurrentDirectory)
import Glob (namesMatching)

renameWith :: (FilePath -> FilePath) -> FilePath -> IO FilePath
renameWith f path = do
    let path' = f path
    rename path path'
    return path'
    
rename :: FilePath -> FilePath -> IO ()
rename old new = do
    isFile <- doesFileExist old
    let f = if isFile then renameFile else renameDirectory
    f old new
    
cc2cpp = mapM (renameWith (flip replaceExtension ".cpp")) =<< namesMatching "*.cc"

-- 1
matchGlob :: FilePath -> String -> Bool
matchGlob  ""     ""      = True
matchGlob  _      ""      = False
matchGlob  ""    (g:gs)
              | g == '*'  = matchGlob ""     gs
              | otherwise = False
matchGlob (p:ps) ('?':gs) = matchGlob ps     gs
matchGlob  path  ('*':gs) = star      path   gs
matchGlob  path  ('[':gs) = cClass    path   gs
matchGlob (p:ps) (g:gs)
              | p == g    = matchGlob ps     gs
              | otherwise = False

star :: FilePath -> String -> Bool
star _   ""      = True
star ""  glob    = matchGlob "" glob
star ps  glob    = matchGlob ps glob || star (tail ps) glob

cClass :: FilePath -> String -> Bool
cClass (p:ps) glob -- path cannot be empty
    | ']' `notElem` glob = error "Undefined character class"
    | ('!':gs) <- glob   = (not (p `inClass` clas)) && matchGlob ps rest
    | otherwise          = (p `inClass` clas) && matchGlob ps rest
        where (clas, bracket:rest) = span (/=']') glob
              inClass :: Char -> String -> Bool
              inClass c cla
                | '-' `elem` cla = inClass c (('[':[(last i)..f])++"]") || inClass c cls
                | otherwise = c `elem` cla
                    where (i, dash:f:cls) = span (/='-') cla
       