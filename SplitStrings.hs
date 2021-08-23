module SplitStrings where
--
--
solution :: String -> [String]
solution []       = [] -- or error. IDK
solution (x:[])   = [[x,'_']]
solution (x:y:xs) = [x,y] : solution xs
--
--
