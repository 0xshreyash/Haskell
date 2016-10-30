data Bool' = True' | False'
                deriving (Show)

not' :: Bool' -> Bool'
not' True' = False'
not' False' = True'

and' :: Bool' -> Bool' -> Bool'
and' False' _ = False'
and' _ False' = False'
and' True' True' = False'

or' :: Bool' -> Bool' -> Bool'
or' True' _ = True'
or' _ True' = True'
or' False' False' = False'
