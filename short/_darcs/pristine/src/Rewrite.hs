module Rewrite where

import Char(toLower)
import Debug.Trace (trace)
import Data.Generics
import Syntax
import Flatten
import ParserHelper(inlineOperatorDef)
import Text.ParserCombinators.Parsec.Pos as PPos
import Language.TLAPlus.Syntax
import SyntaxPretty (ppTy)

---- QUERY --------------------------------------------------------------------

-- the merged spec has one concern and one interaction only.
theInteractionElements :: SH_Spec -> [SH_InteractionElement]
theInteractionElements spec =
    let c = head $ concernList spec
	(SH_Concern _ _ l) = c
	[SH_Interaction _ _ _ _ ies] = 
	    filter (\x -> case x of (SH_Interaction _ _ _ _ _) -> True; 
		                    _ -> False) l
     in ies

roleNames :: [SH_InteractionElement] -> [String]
roleNames l = concat $ map roleNames0 l 
  where roleNames0 (SH_RoleDef _ name _ _) = [name]
	roleNames0 _ = []

allTags :: SH_FL_Spec -> [(String, SH_RoleElement)]
allTags spec = allTags0 $ roleDecl spec
  where allTags0 :: [Role] -> [(String, SH_RoleElement)]
	allTags0 l = concat $ map allTags1 l
	allTags1 :: Role -> [(String, SH_RoleElement)]
	allTags1 (SH_RoleDef _ role _ l) = concat $ map (allTags2 role) l
	allTags1 _ = []
	allTags2 role t@(SH_Tag _ _ _ _ _ _) = [(role, t)]
	allTags2 _ _ = []

{- Wasn't able to carry the role of the tag enclosing structure through ...
   FIXME Need to learn about generic fold 
allTags :: SH_FL_Spec -> [SH_RoleElement]
allTags = everything (++) ([] `mkQ` f)
  where f tag@(SH_Tag _ _ _ _ _) = [tag]
	f _ = []
-}

-- FIXME, don't use typeKernel, simply to match type structure
-- instead (btw. destrole == role doesn't work due to the position
-- elements in the data structure with different line/col numbers)
eqTy a b = typeKernel a == typeKernel b

---- REWRITE ------------------------------------------------------------------
rewriteTag :: SH_FL_Spec -> SH_FL_Spec
rewriteTag = dropTag -- tags are no longer used beyond this weave step 
	   . dropMsgExtend             
	   . rewriteMsgExtend             
	   . rewriteMsg             
           . rewriteSend
	   . rewriteREPLY
           . addBoilerplate

-- turn small caps refences to boilerplace operators into all caps (so TLC
-- can resolve them)
rewriteSpecialOperators :: SH_FL_Spec -> SH_FL_Spec
rewriteSpecialOperators spec = everywhere (mkT f) spec
  where f (AS_OpApp epos (AS_Ident a b "all") l) = 
	    (AS_OpApp epos (AS_Ident a b "ALL") l)
        f (AS_OpApp epos (AS_Ident a b "any") l) = 
	    (AS_OpApp epos (AS_Ident a b "ANY") l)
        f (AS_OpApp epos (AS_Ident a b "any2") l) = 
	    (AS_OpApp epos (AS_Ident a b "ANY2") l)
        f (AS_OpApp epos (AS_Ident a b "senders") l) = 
	    (AS_OpApp epos (AS_Ident a b "SENDERS") l)
	f x = x

dummyI = "dummyInteraction"

addBoilerplate :: SH_FL_Spec -> SH_FL_Spec
addBoilerplate spec =
    let a = SH_VerbTLAOp upos dummyI Nothing genOpNullSender 
	b1 = SH_VerbTLAOp upos dummyI Nothing genOpANY1 
	b2 = SH_VerbTLAOp upos dummyI Nothing genOpANY2
	c = SH_VerbTLAOp upos dummyI Nothing genOpALL
	d = SH_VerbTLAOp upos dummyI Nothing genOpSENDERS
     in spec { verbTLA = a : b1 : b2 : c : d : verbTLA spec }
  where	genOpNullSender = inlineOperatorDef $ unlines 
           (["NullSender(msg) == [msg EXCEPT !.sender = 0]"])
	genOpANY1 = inlineOperatorDef $ unlines 
           (["ANY(s) ==",
	     "  IF Assert(\\A a, b \\in s: NullSender(a) = NullSender(b),",
             "              <<\"ANY(s), not all elements (other than 'sender') are equal!\", s>>)",
             "    THEN NullSender(CHOOSE e \\in s: TRUE)",
	     "    ELSE FALSE"]) -- will never reach this line
	genOpANY2 = inlineOperatorDef $ unlines 
           (["ANY2(s,fieldname) ==",
	     "  LET ss == { val[fieldname] : val \\in s }",
	     "   IN IF Assert(\\A a, b \\in ss: a = b,",
             "                <<\"ANY2(s,fieldname), not all elements are equal!\", s, fieldname, ss>>)",
             "      THEN CHOOSE e \\in ss: TRUE",
	     "      ELSE FALSE"]) -- will never reach this line
	genOpALL = inlineOperatorDef $ unlines 
           (["ALL(s) == s"])
	genOpSENDERS = inlineOperatorDef $ unlines 
           (["SENDERS(s) == { m.sender : m \\in ALL(s) }"])

dropTag :: SH_FL_Spec -> SH_FL_Spec
dropTag spec = spec { roleDecl = concat $ map remTag (roleDecl spec) }
  where remTag :: Role -> [Role]
	remTag (SH_RoleDef p name vars l) = 
	    [SH_RoleDef p name vars (concat $ map remTag0 l)]
	remTag x = [x]
	remTag0 :: SH_RoleElement -> [SH_RoleElement]
        remTag0 (SH_Tag _ _ _ _ _ _) = []
	remTag0 x = [x]

rewriteREPLY :: SH_FL_Spec -> SH_FL_Spec
rewriteREPLY spec = everywhere (mkT f) spec
        -- only applies to handlers of single messages (from = Nothing)
  where f (SH_MsgHandler info ann role when m label any Nothing l) =
	    let l' = map (rewriteREPLY0 role m) l
             in SH_MsgHandler info ann role when m label any Nothing l'
	f x = x
        rewriteREPLY0 :: String -> String -> SH_GuardedInstrList 
		      -> SH_GuardedInstrList
	rewriteREPLY0 role reqm l = everywhere (mkT (g role reqm)) l
	  where g role reqm (SH_I_Reply _ restype ass) = 
		    SH_I_MsgSend1 upos role -- FIXME kramer@acm.org reto --cap?
		      False 
		      False
		      (SH_ExprWrapper upos $ AS_InfixOP epos AS_DOT 
		         (mk_AS_Ident reqm) 
		         (mk_AS_Ident "sender")) 
		      restype ass
	        g _ _ x = x

-- IF the tag matches 'any' message, then find all msg types that can be 
-- sent to the tag's "TO" role (mtypes). Then see if the send-instr at hand
-- is in fact sending a message of any of those types and if so, rewrite
-- the send to include the tags attribute/expr pairs.
-- IF the tag matches a specific message type, see if the send instruction is
-- sending that same message type.
rewriteSend :: SH_FL_Spec -> SH_FL_Spec
rewriteSend spec =
    let tags = allTags spec
	msgdecls = msgDecl spec
     in everywhere (mkT (f tags msgdecls)) spec
  where f tags msgdecls send@(SH_I_MsgSend1 p1 role multi last deste mtype vars) =
	    case isTaggedSend tags send msgdecls of
	      [] -> send
	      add -> SH_I_MsgSend1 p1 role multi last deste mtype (vars ++ add)
	f _ _ x = x

isTaggedSend :: [(String, SH_RoleElement)] -> SH_Instr -> [SH_MsgDecl]
	     -> [(String, SH_ExprWrapper)]
isTaggedSend l m@(SH_I_MsgSend1 p1 role _multi _last destexpr mtype vars) msgdecls =
    -- FIXME presumes only 1 TAG matches, could be list!
    let tags = tagsInRole role l in
        concat $ map f tags
    where f t@(SH_Tag _ _tag_role tag_mtype any tag_to tag_vars) =
	    if (    any 
	         && (elem mtype (msgTypesSendingToDestRole tag_to msgdecls))) 
	    || (    (not any) 
	         && mtype `elem` tag_mtype)
	    then map (\((_t,i), e) -> (i,e)) tag_vars
	    else []
	  msgTypesSendingToDestRole :: SH_Type -> [SH_MsgDecl] -> [String]
	  msgTypesSendingToDestRole role l =
	      map (\(SH_MsgDecl _ _ _ mtype _) -> mtype) $
	      filter (\(SH_MsgDecl _ _ destrole _ _) -> (eqTy destrole role)) l

rewriteMsg :: SH_FL_Spec -> SH_FL_Spec
rewriteMsg spec =
    let tags = allTags spec
     in everywhere (mkT (f tags)) spec
  where f tags m@(SH_MsgDecl p2 from to mtype vars) =
	    case isTaggedMsg tags m of
	      []  -> m
	      add -> SH_MsgDecl p2 from to mtype (vars ++ add)
	f _ x = x

isTaggedMsg :: [(String, SH_RoleElement)] -> SH_MsgDecl 
	    -> [(SH_Type, String)]
isTaggedMsg l (SH_MsgDecl _ from to mtype _) =
    let role = (head $ typeKernel from)
	tags = tagsInRole role l
     in concat $ map f tags
    where f (SH_Tag _ _role tag_mtype any tag_to vars) =
            if (to `eqTy` tag_to) && (any || (mtype `elem` tag_mtype))
            then map (\((t,i),_e) -> (t,i)) vars
            else []

tagsInRole :: String -> [(String, SH_RoleElement)] -> [SH_RoleElement] 
tagsInRole role l =
    map (\(_r, tag) -> tag) (filter (\(r, tag) -> lower r == lower role) l)
    -- FIXME kramer@acm.org reto -- HACK! lower since in the Send1 of
    -- some roles, the role name is capital - that's the real bug
  where lower l = map toLower l

rewriteMsgExtend :: SH_FL_Spec -> SH_FL_Spec
rewriteMsgExtend spec = everywhere (mkT (f spec)) spec
  where f spec m@(SH_MsgDecl p2 from to mtype vars) =
	    let exts = allExtendMsg spec from to mtype
	     in SH_MsgDecl p2 from to mtype (vars ++ exts)
	f _ x = x    
	allExtendMsg spec from to mtype = 
	    everything (++) ([] `mkQ` (h from to mtype)) spec
	  where h from to mtype (SH_Extend_Msg _ from2 to2 mtype2 l) 
	              -- Eq will otherwise incl the position info
		    | (show $ ppTy from) == (show $ ppTy from2) && 
		      (show $ ppTy to) == (show $ ppTy to2) &&
		      mtype == mtype2 = l
		    | otherwise = []
		h _ _ _ _ = []

dropMsgExtend :: SH_FL_Spec -> SH_FL_Spec
dropMsgExtend spec = spec { msgExt = [] }

-- FIXME remove this dup of TLACodeGen
typeKernel (SH_Ty_UserDef _ s) = [s]
typeKernel (SH_Ty_UserDefOrNIL _ t) = typeKernel t
typeKernel (SH_Ty_Expr _ t) = ["anSH_Ty_Expr"]
typeKernel (SH_Ty_SetOf _ t) = typeKernel t
typeKernel (SH_Ty_SeqOf _ t) = typeKernel t
typeKernel (SH_Ty_PairOf _ tA tB) = (typeKernel tA) ++ (typeKernel tB)
typeKernel (SH_Ty_Map _ tA tB) = (typeKernel tA) ++ (typeKernel tB)
typeKernel (SH_Ty_Enum _ l) = l

---- HELPER -------------------------------------------------------------------
mk_AS_Ident s = AS_Ident epos [] s

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos name line col = newPos name line col

upos = mkPos "foo" 0 0 
epos = (upos, Nothing, Nothing)
