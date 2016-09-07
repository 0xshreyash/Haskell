import Data.Char
swapCase::String -> String
swapCase s = map swap s
             where 
             swap c  
                 | isUpper c = toLower c
                 | isLower c = toUpper c
                 | otherwise  = c


