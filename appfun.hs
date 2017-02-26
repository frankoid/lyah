myAction :: IO String
myAction = (++) <$> getLine <*> getLine

myAction' :: IO String
myAction' = (fmap (++) getLine) <*> getLine
