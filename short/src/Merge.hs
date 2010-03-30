module Merge where

import Data.List (nub)
import Text.ParserCombinators.Parsec.Pos as PPos
import Syntax

-- the new concern contains a single interaction with a single instance of ROLE
-- for each distinct role in the input concerns. The interactions and roles are
-- thus merged.
merge :: [SH_Concern] -> SH_Concern
merge concerns =
    let cels = concat $ map (\(SH_Concern _ _ l) -> l) concerns
        constants = filter isConstant cels
	rolelists = filter isRolelist cels
	interaction = mergeInteraction $ filter isInteraction cels
     in SH_Concern upos gen $ concat [constants, rolelists, [interaction]]

mergeInteraction interactions =
    let l = concat $ map mergeInteraction0 interactions
        msgs = filter isMsg l
	exts = filter isMsgExt l
	roles = filter isRole l
	verbs = filter isVerb l
	displayAnn = filter isDisplayAnn l
	names = roleNames roles
        roles' = nub $ [mergeRole n roles | n <- names]
     in SH_Interaction upos gen True [] $ 
	  concat [msgs, displayAnn, exts, verbs, roles']

mergeInteraction0 (SH_Interaction _ _ True {- enabled -} _roles l) = l
mergeInteraction0 _ = []

mergeRole name roles =
    let vl = concat $ map (mergeRole0_vl name) roles
        re = concat $ map (mergeRole0_re name) roles
     in SH_RoleDef upos name vl re

mergeRole0_vl name (SH_RoleDef _ n v _) | name == n = v
mergeRole0_vl _ _ = []

mergeRole0_re name (SH_RoleDef _ n _ re) | name == n = re
mergeRole0_re _ _ = []

gen = "__generated"

---- QUERIES ------------------------------------------------------------------

isConstant :: SH_ConcernElement -> Bool
isConstant (SH_Constant _ _) = True
isConstant _ = False

isRolelist :: SH_ConcernElement -> Bool
isRolelist (SH_RoleList _ _) = True
isRolelist _ = False

isInteraction :: SH_ConcernElement -> Bool
isInteraction (SH_Interaction _ _ _ _ _) = True
isInteraction _ = False

isMsg (SH_IntraInteractionMsg _ _) = True
isMsg _ = False

isDisplayAnn (SH_DisplaySwimlane _ _) = True
isDisplayAnn _ = False

isMsgExt (SH_Extend_Msg _ _ _ _ _) = True
isMsgExt _ = False

isRole (SH_RoleDef _ _ _ _) = True
isRole _ = False

isVerb (SH_VerbTLAOp _ _ _ _) = True
isVerb _ = False

roleNames :: [SH_InteractionElement] -> [String]
roleNames l = concat $ map roleNames0 l 
  where roleNames0 (SH_RoleDef _ name _ _) = [name]
	roleNames0 _ = []

---- HELPER -------------------------------------------------------------------
mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos name line col = newPos name line col

upos = mkPos "foo" 0 0 
epos = (upos, Nothing, Nothing)
