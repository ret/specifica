module RewriteMsgSetHandler(rewriteMsgSetHandler) where

import Data.Char(toLower)
import Data.List( (\\) )
import Data.Generics
import Syntax
import Flatten
import TLACodeGen(mk_AS_Type, combineInfix, mkVar, substSH_Instr, typeKernel)
import Debug.Trace(trace)
import RewriteExtendHook(substGIL)
import RewriteCont(beautifyLAND)

import Text.ParserCombinators.Parsec.Pos as PPos
import Language.TLAPlus.Syntax as TLASyntax

rewriteMsgSetHandler :: SH_FL_Spec -> SH_FL_Spec
rewriteMsgSetHandler = 
      beautifyLAND
    . glueContinue 
    . rewriteDomi
    . instantiateSpecificAnyHandlers
    . augmentNonExplicitMsgHandlers
    . rewriteAnyHandlerGuard


glueContinue :: SH_FL_Spec -> SH_FL_Spec
glueContinue spec = everywhere (mkT f) spec
    where f (SH_MsgHandler p ann role when mbind glblHooks 
	       True {- ANY, we've marked up the handler temporarly -} 
	       from ginstr) =
	      let ginstr' = addHooksIfNoBreak
			      [SH_HookCaller upos 
			         (mkHookName mbind) 
			         [SH_ExprWrapper upos $ mk_AS_Ident mbind]]
			      ginstr
		  ginstr'' = (dropCONTINUE . dropBREAK) ginstr'
		  -- turn of the tagging ANY flag!
	       in SH_MsgHandler p ann role when mbind glblHooks False from 
		       ginstr''
	  f x = x

augmentNonExplicitMsgHandlers :: SH_FL_Spec -> SH_FL_Spec
augmentNonExplicitMsgHandlers spec =  everywhere (mkT (f spec)) spec
    where f spec roledef@(SH_RoleDef _ role vars elems) =
	      if hasAny elems
	      -- generate an empty singular handler (no ANY) for each
	      -- msg type that could be send to role which does not already
	      -- have an explicit singular handler.
	      then let allMsgSentToRole = allMTypesToRole spec role
		       msgWithSingularHandler = singularHandlerMsgs elems
		       missingMType = 
			   allMsgSentToRole \\ msgWithSingularHandler
		       missingH = map (mkEmptyMsgHandler role) missingMType
	            in SH_RoleDef upos role vars (elems ++ missingH)
	      else roledef
	  f _ x = x
	  mkEmptyMsgHandler role mtype =
	    SH_MsgHandler upos [] (map toLower role) 
	      Nothing mtype Nothing False Nothing
	      [SH_GuardedInstrList upos Nothing Nothing 
	         [SH_I_FailTLAClause upos ]] -- do nothing, AND don't drop msg!
          hasAny :: [SH_RoleElement] -> Bool
	  hasAny l = [] /= (everything (++) ([] `mkQ` f)) l
	      where f a@(SH_MsgHandler _ _ r _ _ _ True Nothing _) = [True]
		    f _ = []
          singularHandlerMsgs :: [SH_RoleElement] -> [String]
	  singularHandlerMsgs = everything (++) ([] `mkQ` f)
	      where f a@(SH_MsgHandler _ _ r _ mtype _ False Nothing _) = [mtype]
		    f _ = []
	  allMTypesToRole :: SH_FL_Spec -> String -> [String]
	  allMTypesToRole spec role = 
	      (everything (++) ([] `mkQ` (f role))) spec
	      where f role (SH_MsgDecl _ _ to mtype _) 
		          -- note: to can only be UserDefType of Set<UDef>
		          -- so assuming that the kernel is a single string is
		          -- safe (i.e. no one can send msgs to e.g. map<.,.>
			| typeKernel to == [role] = [mtype]
			| otherwise  = []
		    f _ _ = []

instantiateSpecificAnyHandlers :: SH_FL_Spec -> SH_FL_Spec
instantiateSpecificAnyHandlers spec = everywhere (mkT (f spec)) spec
    where f spec roledef@(SH_RoleDef _ role vars elems) =
	      let anyHList = dominatingHandler spec (map toLower role)
	       in if anyHList == []
		  then roledef
		  else
		      -- *** comment
		      -- a 2nd set of specific message handlers is generated
		      -- they are "marked" by setting the ANY flag (hack)
		      -- such that a subsequent pass over the set of all 
		      -- handlers (elems/old ++ elems'/new) will still be
		      -- able to turn the old specific handlers into @extend
		      -- templates that are then in yet another pass injected
		      -- into the new (elems') handlers before those are
		      -- turned back into specific handlers (clear ANY flag) 
		      let elems' = map (\anyH -> 
					  concat $ 
					     map (instantiateMsgHandler anyH) 
	                                         elems) anyHList
			  l = elems ++ concat elems'
	               in SH_RoleDef upos role vars (l \\ anyHList)
	  f _ x = x

-- for guarded ANY handlers, add a gil leg such that the negated guard is
-- covered also (with a pass-through 'continue')
rewriteAnyHandlerGuard :: SH_FL_Spec -> SH_FL_Spec
rewriteAnyHandlerGuard spec = everywhere (mkT f) spec
    where f (SH_MsgHandler _ ann role when mbind glblHooks 
	       True    {- ANY -} 
	       Nothing {- No majority/all -} 
	       ginstr) =
	    let negLeg = case when of
			   Nothing -> []
		           Just _ -> [SH_GuardedInstrList upos
				        (negate when) 
				        Nothing 
				        [SH_I_Continue upos]]
		-- move guard inside, add neg guard leg
		-- add the new leg at the front since if we added it at the
		-- end, we might add a case arm _after_ a user defined
		-- "otherwise" which TLACodeGen cannot handle right now
		-- (assumes the "otherwise" is the last branch, otherwise
		-- it doesn't rewrite it)
                ginstr' = negLeg ++ (addGuards [when] ginstr)
	     in SH_MsgHandler upos ann role Nothing {- clear guard, moved inside -}
		  mbind glblHooks True Nothing ginstr'
	  f x = x
          negate Nothing = Nothing
	  negate (Just (SH_ExprWrapper _ e)) = 
	      Just $ SH_ExprWrapper upos (AS_PrefixOP epos AS_Not e)

instantiateMsgHandler :: SH_RoleElement -> SH_RoleElement -> [SH_RoleElement]
instantiateMsgHandler anyH (SH_MsgHandler _ ann role when mbind glblHooks 
		              False {- match non ANY, i.e. specific -} 
		              Nothing {- don't apply ANY pattern to Maj/All -} 
			      ginstr) =
  let SH_MsgHandler _ ann arole Nothing {- guard was moved inside with
					   rewriteAnyHandlerGuard ! -} 
		         amtype aglblHooks True Nothing aginstr = anyH 
      -- replace ANY m with actual mtype of specific handler
      gil' = substGIL [(amtype, SH_ExprWrapper upos (mk_AS_Ident mbind))]
	       aginstr 
   in [SH_MsgHandler upos ann role when mbind glblHooks 
         True {- HACK HACK mark generated handler with ANY flag, see *** -}
         Nothing {- don't apply ANY pattern to Maj/All -} 
         gil' {- TRANSPLANT the body of the ANY handler -} ]
instantiateMsgHandler _ _ = []

addHooksIfNoBreak :: [SH_HookCaller] -> [SH_GuardedInstrList] 
		  -> [SH_GuardedInstrList]
addHooksIfNoBreak hooks l = map (addHooksIfNoBreak0 hooks) l
  where addHooksIfNoBreak0 hooks (SH_GuardedInstrList _ guard h l) =
            let h' = if hasBreak l 
		     then h 
		     else case h of 
		            Nothing -> Just hooks
			    (Just htmp) -> Just $ htmp ++ hooks
	     in SH_GuardedInstrList upos guard h' l
        hasBreak = any isBREAK

rewriteDomi :: SH_FL_Spec -> SH_FL_Spec
rewriteDomi spec = everywhere (mkT (f spec)) spec
    where f spec h@(SH_MsgHandler _ ann role when mbind glblHooks 
		    False Nothing ginstr) =
	    if dominatingHandler spec role /= [] 
	    then mkExtend spec h 
	    else h
	  f _ x = x

dropBREAK :: [SH_GuardedInstrList] -> [SH_GuardedInstrList]
dropBREAK l = map remBREAK l
  where remBREAK (SH_GuardedInstrList _ guard hooks l) =  
	    SH_GuardedInstrList upos guard hooks (filter (not . isBREAK) l)

isBREAK (SH_I_Break _) = True
isBREAK _ = False

dropCONTINUE :: [SH_GuardedInstrList] -> [SH_GuardedInstrList]
dropCONTINUE l = map remCONTINUE l
  where remCONTINUE (SH_GuardedInstrList _ guard hooks l) =  
	    SH_GuardedInstrList upos guard hooks (filter (not . isCONTINUE) l)
	isCONTINUE (SH_I_Continue _) = True
	isCONTINUE _ = False

rewriteANY :: SH_FL_Spec -> SH_FL_Spec
rewriteANY spec = everywhere (mkT (f spec)) spec
    where f spec h@(SH_MsgHandler _ ann role when mbind glblHooks 
		    True from ginstr) = -- ANY handler
	    let mtypes = dominatedMsgTypes spec role
		hs = map (\mtype -> SH_HookCaller upos 
		              (mkHookName mtype) 
		              [SH_ExprWrapper upos $ mk_AS_Ident mbind])
		     mtypes
	     in if mtypes /= [] 
		then let ginstr' = addHooksIfNoBreak hs ginstr
		         ginstr'' = (dropCONTINUE . dropBREAK) ginstr'
		      in SH_MsgHandler upos ann role when mbind glblHooks
			               True from ginstr'' 
		else h
	  f _ x = x





-- As long as we only support a single ANY msg handler per role, we can just
dominatedMsgTypes :: SH_FL_Spec -> String -> [String]
dominatedMsgTypes spec role = (everything (++) ([] `mkQ` (f role))) spec
  where f role (SH_MsgHandler _ _ r _ mtype _ False Nothing _) -- no ANY, no FROM
	  | r == role = [mtype]
	  | otherwise = []
	f _ _ = []

dominatingHandler :: SH_FL_Spec -> String -> [SH_RoleElement]
dominatingHandler spec role = 
    (everything (++) ([] `mkQ` (f role))) spec
  where f role
	  a@(SH_MsgHandler _ _ r _ _ _ True Nothing _)
	      | r == role = [a] -- non-ANY handler, no FROM, inside same role
	      | otherwise = []
	f _ _ = []

-- FIXME kramer@acm.org reto -- what if glblHooks isn't Nothing?
mkExtend :: SH_FL_Spec -> SH_RoleElement -> SH_RoleElement
mkExtend spec (SH_MsgHandler i ann role when mtype glblHooks a from ginstr) =
    let mtypeGuard = (AS_InfixOP epos AS_EQ
		      (AS_InfixOP epos AS_DOT 
		       (mk_AS_Ident mtype)
		       (mk_AS_Ident "type"))
		      (mk_AS_Ident $ show mtype))
        guards = [Just $ SH_ExprWrapper upos mtypeGuard, 
		  when]
	ginstr' = addGuards guards ginstr
        -- FIXME kramer@acm.org reto -- need to add check for m.type 
        -- to existing when AND need to fold that into existing guarded
        -- instructions
     in SH_Extend_Hook i role [SH_HookCallee upos 
			       (mkHookName mtype) [mtype]] ginstr

mkHookName s = "any_hook_" ++ s

addGuards :: [(Maybe SH_ExprWrapper)] -> [SH_GuardedInstrList] 
	  -> [SH_GuardedInstrList]
addGuards guards l = map f l
  where f gil@(SH_GuardedInstrList _ g hooks instrs) =
	    case g of
	      (Just (SH_ExprWrapper _ (AS_Ident _ _ "otherwise"))) ->
		  gil -- if guard was "otherwise ->", do not alter!
	      otherwise ->
		  SH_GuardedInstrList upos (combineGuards (guards ++ [g])) 
				      hooks instrs

combineGuards :: [Maybe SH_ExprWrapper] -> (Maybe SH_ExprWrapper)
combineGuards l = let gs = dropNothing l
		   in if gs == []
		      then Nothing
		      else Just $ SH_ExprWrapper upos (AS_LAND epos gs)

dropNothing :: [Maybe SH_ExprWrapper] -> [AS_Expression]
dropNothing l = let l' = filter (/= Nothing) l
                 in map (\(Just (SH_ExprWrapper _ e)) -> e) l'

---- HELPER -------------------------------------------------------------------
mk_AS_Ident s = AS_Ident epos [] s

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos name line col = newPos name line col

upos = mkPos "foo" 0 0 
epos = (upos, Nothing, Nothing)
