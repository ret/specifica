module TraceReader where

import Data.List ((\\))
import qualified Data.List as List
import Data.Map ((!), Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Debug.Trace as Trace
import System.IO
import Lexer (alexScanTokens)
import Parser (Stmt(..), Ident(..), Expr(..), exprparser)
import Text.Regex as Regex
import Debug.Trace(trace)

import ExprHelper
import Language.TLAPlus.Eval (ThrowsError(..))

data Decoration = Color String
                | Style String 
                | Label String
                | LabelFont String
                | TipLabel String 
                | TipLabelFont String 
                | HideDiff [String] deriving (Show, Eq, Ord)
type RFieldName = String
type SwimlaneName = String
type CompDecoration = ([Decoration], (Maybe SwimlaneName, Maybe SwimlaneName))
type DecorationFun = Expr -> (Int, String) -> (Int, String) 
                     -> (Path, Path, Int) -> Maybe CompDecoration
type StateAnnFun = Expr -> Maybe [Decoration]

type SLAMsg = ((Int, String), (Int, String), [Decoration])
type SLAFun =  [(Int, Expr)] -> [SLAMsg]

data Pattern = StaticPattern  RFieldName RFieldName [Decoration]
             | DynamicPattern RFieldName RFieldName DecorationFun

type State = (Int, [Stmt]) -- 0 is initial state
type DiffGroup = [DiffDescr]
data PathRef = VarRef String
             | RecFieldRef String
             | MapKeyRef String 
             | SeqPosRef Int deriving (Show, Eq, Ord)
type Path = [PathRef]
data DDQualifier = AtHead | AtTail deriving (Show, Eq, Ord)
data DiffDescr = DChg Path Expr Expr
               | DAdd (Maybe DDQualifier) Path Expr
               | DRem (Maybe DDQualifier) Path Expr deriving (Show, Eq, Ord)
type StateChange = (Int, [(Ident, DiffGroup)]) -- int is next state index

type MsgExchange = ((Int,SwimlaneName), (Int,SwimlaneName), [Decoration])

getPathRef :: DiffDescr -> Int -> String
getPathRef (DChg p _ _) i = stringPathRef $ p!!i
getPathRef (DAdd _ p _) i = stringPathRef $ p!!i
getPathRef (DRem _ p _) i = stringPathRef $ p!!i

getPath :: DiffDescr -> Path
getPath (DChg p _ _) = p
getPath (DAdd _ p _) = p
getPath (DRem _ p _) = p

stringPathRef :: PathRef -> String
stringPathRef (VarRef s     ) = s
stringPathRef (RecFieldRef s) = s
stringPathRef (MapKeyRef   s) = s
stringPathRef (SeqPosRef   i) = show i

includesAnyPathRef :: [String] -> DiffDescr -> Bool
includesAnyPathRef refs dd = 
    let path = getPath dd
        ref = List.find (\pr -> elem (stringPathRef pr) refs) path
     in Nothing /= ref

diffSL :: State -> State -> StateChange
diffSL (idx, sl) (idx', sl') = 
  let l = map (\s -> let Bind i e = s
                         Ident id = i
                      in case lookupS i sl' of
                           Just (Bind _i' e') -> (i, diffE [VarRef id] e e')
                           Nothing -> (i, [])) sl
   in (idx', l)
  where lookupS :: Ident -> [Stmt] -> Maybe Stmt
        lookupS i (Bind id e:sl) | i == id   = Just (Bind id e)
                                 | otherwise = lookupS i sl
        lookupS i [] = Nothing

diffE :: Path -> Expr -> Expr -> DiffGroup
diffE p (RecE m) (RecE m') = diffRec p m m'
diffE p (MapE m) (MapE m') = diffMap p m m'
diffE p (SetE s) (SetE s') = diffSet p s s'
diffE p (SeqE s) (SeqE s') = diffSeq p s s'
diffE p e1 e2 | (getS e1) == (getS e2) = []
              | otherwise = [DChg p e1 e2]
  where getS :: Expr -> String
        getS (IntE  i) = show i
        getS (StrE  s) = show s
        getS (AtomE s) = show s
        getS x = trace (">>>> " ++ show x) "error"

diffRec p m m' =
  let km  = Map.keys m
      km' = Map.keys m'
      krem = km  \\ km'
      kadd = km' \\ km
      kchg = km `List.intersect` km'
   in concat [map (\k -> DRem Nothing (p++[rfr k]) (m!k)) krem,
              map (\k -> DAdd Nothing (p++[rfr k]) (m'!k)) kadd,
              concat $ map (\k -> diffE (p++[rfr k]) (m!k) (m'!k)) kchg]
   where rfr (Ident f) = RecFieldRef f

diffMap p m m' =
  let km  = Map.keys m
      km' = Map.keys m'
      krem = km  \\ km'
      kadd = km' \\ km
      kchg = km `List.intersect` km'
   in concat [map (\k -> DRem Nothing (p++[mkr k]) (m!k)) krem,
              map (\k -> DAdd Nothing (p++[mkr k]) (m'!k)) kadd,
              concat $ map (\k -> diffE (p++[mkr k]) (m!k) (m'!k)) kchg]
   where mkr k = MapKeyRef $ ppE k Map.empty

diffSet p s s' =
  let srem = Set.elems $ s  Set.\\ s'
      sadd = Set.elems $ s' Set.\\ s
      -- there's no notion of a changed element in a set since we know of no
      -- way to refer to a specific set element by reference, a "change" will
      -- really be a removal and addition.
    in concat [map (\e -> DRem Nothing p e) srem,
                map (\e -> DAdd Nothing p e) sadd]

diffSeq p s s' =
  if length s == length s'
  then concat $ map (\k -> diffE (p++[spr k]) (s!!k) (s'!!k)) [0..length s-1]
  else if s `List.isPrefixOf` s'
       then prefixA p s s'
       else if s `List.isSuffixOf` s'
            then suffixA p s s'
            else if s' `List.isPrefixOf` s
                 then prefixR p s s'
                 else if s' `List.isSuffixOf` s
                      then suffixR p s s'
                      else overlap p s s'
  where spr idx = SeqPosRef (idx+1) -- TLA sequences start with index 1
        overlap p s s' =
          let ks   = [0 .. length s -1]
              ks'  = [0 .. length s'-1]
              krem = ks  \\ ks'
              kadd = ks' \\ ks
              kchg = ks `List.intersect` ks'
           in concat [map (\k -> DRem Nothing (p++[spr k]) (s!!k)) krem,
                      map (\k -> DAdd Nothing (p++[spr k]) (s'!!k)) kadd,
                      concat $ map (\k -> diffE (p++[spr k]) (s!!k) (s'!!k)) 
                                   kchg]
        prefixA p s s' =                                  -- s  0 1 2
          let ks = [length s .. length s'-1]              -- s' 0 1 2 3 4
           in map (\k -> DAdd (Just AtTail) (p++[spr k]) (s'!!k)) ks
        suffixA p s s' =                                  -- s      0 1 2
          let ks = [0 .. (length s' - length s)-1]        -- s' 0 1 2 3 4
           in map (\k -> DAdd (Just AtHead) (p++[spr k]) (s'!!k)) ks
        prefixR p s s' =                                  -- s  0 1 2 3 4
          let ks = [length s' .. length s-1]              -- s' 0 1 2
           in map (\k -> DRem (Just AtTail) (p++[spr k]) (s!!k)) ks
        suffixR p s s' =                                  -- s  0 1 2 3 4
          let ks = [0 .. (length s - length s')-1]        -- s'     0 1 2
           in map (\k -> DRem (Just AtHead) (p++[spr k]) (s!!k)) ks

printPathRef :: PathRef -> String
printPathRef (VarRef      r) = r
printPathRef (RecFieldRef r) = "."++r
printPathRef (MapKeyRef   r) = "["++r++"]"
printPathRef (SeqPosRef   r) = "["++show r++"]"

printPath :: Path -> String
printPath p = concat $ map printPathRef p

ppDL :: [DiffDescr] -> [String]
ppDL = map ppD

ppD :: DiffDescr -> String
ppD (DChg p e e')  = (printPath p)++" $\\mapsto$ "++(ppE e  Map.empty)++
                                    " / " ++(color "blue" $ ppE e' Map.empty)
ppD (DAdd q p e )  = (printPath p)++" $\\mapsto$ +"++printQ q++" "++
                     color "green" (ppE e Map.empty)
ppD (DRem q p e )  = (printPath p)++" $\\mapsto$ -"++printQ q++" "++
                     color "red" (ppE e Map.empty)

printQ Nothing = ""
printQ (Just AtHead) = "h"
printQ (Just AtTail) = "e"
