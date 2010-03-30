module Main where

d = [-1, 1,2,3, -2,4,5,6, -3,7,8,9]

type Section a = (a, [a])

-- assumes (test $ head l), i.e. the first element in the list is a header
chop :: Eq a => (a -> Bool) -> [a] -> [Section a] -> [Section a]
chop _ [] _ = []
chop test l acc = case chop0 test (head l) (tail l) [] of
	            (section, [])    -> acc ++ [section]
	            (section, rest)  -> chop test rest (acc ++ [section])

chop0 :: Eq a => (a -> Bool) -> a -> [a] -> [a] -> (Section a, [a])
chop0 test head []         acc             = ((head, acc), [])
chop0 test head l@(h:rest) acc | test h    = ((head, acc), l)
			       | otherwise = chop0 test head rest (acc ++ [h])

main :: IO ()
main = do putStrLn $ show d
	  putStrLn $ show (chop (< 0) d [])