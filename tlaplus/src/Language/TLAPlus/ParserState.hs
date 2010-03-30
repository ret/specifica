module Language.TLAPlus.ParserState 
    ( TLAParser
    , PState, mkState, pushIndent, popIndent, peekIndent
    , setRole, getRole, setInteraction, getInteraction ) where

import Data.Generics
import Text.ParserCombinators.Parsec

data PState = PState 
       { indent      :: [Int]
       , role        :: String -- FIXME, is for australis/short only
       , interaction :: String -- FIXME, is for australis/short only
       } deriving (Eq, Ord, Show, Data, Typeable)
type TLAParser a = GenParser Char PState a

mkState :: PState
mkState = PState 
            { indent = []
            , role = "?ParserState_NoRoleAssigned" 
            , interaction = "?ParserState_NoInteractionAssigned" 
            }

pushIndent :: PState -> Int -> PState
pushIndent st i = st { indent = i:(indent st) }

popIndent :: PState -> (Int, PState)
popIndent st = let l = indent st
                   st' = st { indent = tail l }
                in (head l, st')

peekIndent :: PState -> Maybe Int
peekIndent st = let l = indent st
                 in case l of
                      [] -> Nothing
                      _ -> Just $ head l

-- FIXME, is for australis/short only
setRole :: PState -> String -> PState
setRole st s = st { role = s }

-- FIXME, is for australis/short only
getRole :: PState -> String
getRole st = role st

-- FIXME, is for australis/short only
setInteraction :: PState -> String -> PState
setInteraction st s = st { interaction = s }

-- FIXME, is for australis/short only
getInteraction :: PState -> String
getInteraction st = interaction st
