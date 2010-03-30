module ExprHelper where

import Parser (Ident(..), Expr(..))
import qualified Data.List as List
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

join sep []  = ""
join sep [s] = s
join sep l   = let rl = reverse l
                in foldl (\e acc -> acc++sep++e) (head rl) (tail rl)

type Abbrev = Map String String
ppE :: Expr -> Abbrev -> String
ppE (RecE map) a = let l = Map.foldWithKey 
			    (\k e acc -> ((ppI k a)++"="++(ppE e a)):acc)
			    [] map
 		   in "["++(join ", " l)++"]"
ppE (MapE map) a = let l = Map.foldWithKey 
			    (\k e acc -> ((ppE k a)++"="++(ppE e a)):acc)
			    [] map
 		   in "["++(join ", " l)++"]"
ppE (SetE se) a = let l = Set.fold (\e acc -> (ppE e a):acc) [] se
		   in "\\{"++(join ", " l)++"\\}"
ppE (SeqE le) a = let l = List.foldl (\acc e -> (ppE e a):acc) [] le
		   in "$\\ll$"++(join ", " l)++"$\\gg$"
ppE (IntE i)  a = shortform (show i) a
ppE (StrE s)  a = "{\\tt \"" ++ (shortform s a) ++ "\"}"
ppE (AtomE s) a = shortform s a

ppI :: Ident -> Abbrev -> String
ppI (Ident i) a = shortform i a

shortform s abbrev = 
    case Map.lookup s abbrev of
      Nothing -> case Map.lookup "OTHERWISE" abbrev of
                   Nothing -> s -- pass through plain
	           Just o  -> o -- catch all clause
      Just short -> short

-- used for printing of message records
ppRec :: Expr -> Abbrev -> String
ppRec (RecE map) a = let l = Map.foldWithKey 
			     (\k e acc -> ((bf $ ppI k a)++"="++(ppE e a)):acc)
			     [] map
 		      in "["++(join ", " l)++"]"

box s = "\\fbox{"++s++"}"
bf s = "{\\bf "++s++"}"
sl s = "{\\em "++s++"}"
color c s = "{\\color{"++c++"}"++s++"}"

