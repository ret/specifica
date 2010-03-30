module RewriteExtendHook where

import Data.Generics
import Data.Typeable as Typeable (typeOf)
import Syntax
import Flatten
import Data.List(groupBy, nub)
import TLACodeGen(mk_AS_Type, combineInfix, mkVar, substSH_Instr)
import Debug.Trace(trace)
import RewriteCont(beautifyLAND)
import Flatten(prettyPrintFlatSH)

import Text.ParserCombinators.Parsec.Pos(newPos, SourcePos(..))
import Language.TLAPlus.Syntax as TLASyntax

rewriteExtendHook :: SH_FL_Spec -> SH_FL_Spec
rewriteExtendHook = beautifyLAND               -- . (tp "4") 
--		  . pruneEmptyInstructionLists -- . (tp "3") 
		  . dropEXT_HOOK               -- . (tp "2") 
		  . hackDeDup
                  . mergeExtendHook            -- . (tp "1") 
                  . mergeEXT_HOOK              -- . (tp "0")
		  -- after the split each EXTEND statement is guaranteed to
		  -- contain only a singleton list of callees
		  . splitEXT_HOOK              -- . (tp "-1")

tp :: String -> SH_FL_Spec -> SH_FL_Spec
tp label spec = trace (">>>> " ++ label ++ prettyPrintFlatSH spec) spec

pruneEmptyInstructionLists :: SH_FL_Spec -> SH_FL_Spec
pruneEmptyInstructionLists spec = everywhere (mkT f) spec
    where f h@(SH_MsgHandler _ ann role when mtype glblHooks any from ginstr) =
	      SH_MsgHandler upos ann role when mtype Nothing any from
		(purgeEmpty ginstr)
	  f (SH_CallHandler _ role when label args glblHooks ginstr) =
	      SH_CallHandler upos role when label args Nothing
		(purgeEmpty ginstr)
	  f (SH_TimeoutHandler _ role when name glblHooks ginstr) =
	      SH_TimeoutHandler upos role when name Nothing
		(purgeEmpty ginstr)
	  f (SH_CrashHandler _ ann role when remoteRole id glblHooks ginstr) =
	      SH_CrashHandler upos ann role when remoteRole id Nothing
		(purgeEmpty ginstr)
	  f (SH_Every _ role guard timer glblHooks ginstr) =
	      SH_Every upos role guard timer Nothing
		(purgeEmpty ginstr)
	  f (SH_Extend_Hook _ role callee ginstr) =
	      SH_Extend_Hook upos role callee 
		(purgeEmpty ginstr)
	  f (SH_Once _ role guard label glblHooks ginstr) =
	      SH_Once upos role guard label glblHooks 
		(purgeEmpty ginstr)
	  f x = x
          purgeEmpty l = filter (not . hasEmptyIL) l
	  hasEmptyIL (SH_GuardedInstrList _ _ _ []) = True
	  hasEmptyIL _ = False

mergeExtendHook :: SH_FL_Spec -> SH_FL_Spec
mergeExtendHook spec = fixP mergeExtendHook0 spec 
    -- note that we're clearing the hooks after the rewrite to indicate they
    -- where merged.
    where mergeExtendHook0 spec = everywhere (mkT f) spec
          f h@(SH_MsgHandler _ ann role when mtype glblHooks any from ginstr) =
	      SH_MsgHandler upos ann role when mtype Nothing any from
		(spliceHooks spec glblHooks ginstr) 
	  f (SH_CallHandler _ role when label args glblHooks ginstr) =
	      SH_CallHandler upos role when label args Nothing
		(spliceHooks spec glblHooks ginstr)
	  f (SH_TimeoutHandler _ role when name glblHooks ginstr) =
	      SH_TimeoutHandler upos role when name Nothing
		(spliceHooks spec glblHooks ginstr)
	  f (SH_CrashHandler _ ann role when remoteRole id glblHooks ginstr) =
	      SH_CrashHandler upos ann role when remoteRole id Nothing
		(spliceHooks spec glblHooks ginstr)
	  f (SH_Every _ role guard timer glblHooks ginstr) =
	      SH_Every upos role guard timer Nothing
		(spliceHooks spec glblHooks ginstr)
	  -- NOTE that ExtendHooks thenselves cannot contain other hooks,
	  -- thus there's not case to handle them.
	  f (SH_Once _ role guard label glblHooks ginstr) =
	      SH_Once upos role guard label glblHooks 
		(spliceHooks spec glblHooks ginstr)
	  f x = x
	  fixP f arg = if f arg == arg
		       then arg
		       else fixP f (f arg)

spliceHooks :: SH_FL_Spec -> (Maybe [SH_HookCaller]) -> [SH_GuardedInstrList]
	    -> [SH_GuardedInstrList]
spliceHooks spec (Just glblHooks) l =
    let blocks = map (findAndSubstBindings spec) glblHooks
     in appendSpliceHookGIL spec (concat blocks) l
spliceHooks spec Nothing l = -- no global hook, there may be per branch hooks!
    appendSpliceHookGIL spec [] l

-- FIXME kramer@acm.org reto -- we should only find the hooks in the same role.
findAndSubstBindings :: SH_FL_Spec -> SH_HookCaller -> [SH_GuardedInstrList]
findAndSubstBindings spec (SH_HookCaller _ label argList) =
    (everything (++) ([] `mkQ` (f label argList))) spec
  where f label argList 
          (SH_Extend_Hook _ _ [SH_HookCallee _ calleeLabel calleeArgList] l) =
	    if (label == calleeLabel) && 
		   ((length argList) == (length calleeArgList))
	    then substGIL (zip calleeArgList argList) l
	    else []
	f _ _ _ = []

appendSpliceHookGIL :: SH_FL_Spec 
		    -> [SH_GuardedInstrList] -> [SH_GuardedInstrList] 
		    -> [SH_GuardedInstrList] 
appendSpliceHookGIL spec extil l = 
    concat $ map (appendSpliceHookGIL0 spec extil) l
  where 
    appendSpliceHookGIL0 :: SH_FL_Spec 
			 -> [SH_GuardedInstrList] -> SH_GuardedInstrList 
			 -> [SH_GuardedInstrList]
    appendSpliceHookGIL0 spec extil 
			 g@(SH_GuardedInstrList i guard Nothing l) =
        -- no branch local hooks, add global ones only (extil) if existent
	if extil == []
	then [g]
	else map (\(SH_GuardedInstrList _ w h il) -> 
	               SH_GuardedInstrList i (mkLAND guard w) h (l ++ il)
		 ) extil
    appendSpliceHookGIL0 spec extil 
			 g@(SH_GuardedInstrList i guard (Just hs) l) =
	let hextil = concat $ map (findAndSubstBindings spec) hs
	    h = extil ++ hextil
	 in if h == []
	    then [g]
	    else map (\(SH_GuardedInstrList _ w h il) -> 
	               SH_GuardedInstrList i (mkLAND guard w) h (l ++ il)
		     ) h

-- FIXME kramer@acm.org reto -- I'm sure there's a monad call to get rid of N.
mkLAND (Just (SH_ExprWrapper _ a)) (Just (SH_ExprWrapper _ b)) = 
    Just $ SH_ExprWrapper upos (AS_LAND epos [a,b])
mkLAND Nothing ew@(Just (SH_ExprWrapper _ _e)) = ew
mkLAND ew@(Just (SH_ExprWrapper _ _e)) Nothing = ew
mkLAND Nothing Nothing = Nothing

substGIL :: [(String, SH_ExprWrapper)] -> [SH_GuardedInstrList]
	    -> [SH_GuardedInstrList]
substGIL l = everywhere (mkT f)
  where f i@(AS_Ident _ _ s) =
	    case lookup s l of Nothing -> i
			       Just w  -> case w of SH_ExprWrapper _ e -> e
        f x = x

splitEXT_HOOK :: SH_FL_Spec -> SH_FL_Spec
splitEXT_HOOK spec = 
    spec { roleDecl = concat $ map splEXT_HOOK (roleDecl spec) }
  where splEXT_HOOK :: Role -> [Role]
	splEXT_HOOK (SH_RoleDef p name vars l) = 
	    [SH_RoleDef p name vars (concat $ map splEXT_HOOK0 l)]
	splEXT_HOOK x = [x]
	splEXT_HOOK0 :: SH_RoleElement -> [SH_RoleElement]
        splEXT_HOOK0 (SH_Extend_Hook i r l gils) =
	    map (\c -> SH_Extend_Hook i r [c] gils) l
	splEXT_HOOK0 x = [x]

dropEXT_HOOK :: SH_FL_Spec -> SH_FL_Spec
dropEXT_HOOK spec = 
    spec { roleDecl = concat $ map remEXT_HOOK (roleDecl spec) }
  where remEXT_HOOK :: Role -> [Role]
	remEXT_HOOK (SH_RoleDef p name vars l) = 
	    [SH_RoleDef p name vars (concat $ map remEXT_HOOK0 l)]
	remEXT_HOOK x = [x]
	remEXT_HOOK0 :: SH_RoleElement -> [SH_RoleElement]
        remEXT_HOOK0 (SH_Extend_Hook _ _ _ _) = []
	remEXT_HOOK0 x = [x]

mergeEXT_HOOK :: SH_FL_Spec -> SH_FL_Spec
mergeEXT_HOOK spec = 
    spec { roleDecl = concat $ map merEXT_HOOK (roleDecl spec) }
  where merEXT_HOOK :: Role -> [Role]
	merEXT_HOOK (SH_RoleDef p name vars l) = 
	    [SH_RoleDef p name vars (merEXT_HOOK0 l)]
	merEXT_HOOK x = [x]
	merEXT_HOOK0 :: [SH_RoleElement] -> [SH_RoleElement]
        merEXT_HOOK0 l = 
	    let el  = filter isExt l
		nel = filter (not . isExt) l
             in nel ++ (combineEXT_HOOK . groupEXT_HOOK) el
        isExt (SH_Extend_Hook _ _ _ _) = True
	isExt _ = False

groupEXT_HOOK :: [SH_RoleElement] -> [[SH_RoleElement]]
groupEXT_HOOK = groupBy (\a b -> sameHook a b)
  where sameHook (SH_Extend_Hook _ _ [SH_HookCallee _ na aargs] _) 
                 (SH_Extend_Hook _ _ [SH_HookCallee _ nb bargs] _) =
	  (na == nb) && (length aargs == length bargs)
	sameHook _ _ = False

combineEXT_HOOK :: [[SH_RoleElement]] -> [SH_RoleElement]
combineEXT_HOOK l = concat $ map comb l
  where comb :: [SH_RoleElement] -> [SH_RoleElement]
	comb [] = []
	comb l@((SH_Extend_Hook _ r [SH_HookCallee _ name args] _):_) =
	    let il  = mergeIL $ concatMap mkIL l
		il' = remDupHACK il
		args' = map mkArg [1..length args]
             in [SH_Extend_Hook upos r [SH_HookCallee upos name args'] il']
        mkIL :: SH_RoleElement -> [SH_GuardedInstrList] 
	mkIL (SH_Extend_Hook _ _ [SH_HookCallee _ _ args] l) = 
	    substGIL (mkArgs args) l
        mkArgs :: [String] -> [(String, SH_ExprWrapper)]
	mkArgs l = map (\(s,i) -> 
		          (s, SH_ExprWrapper upos $ mk_AS_Ident (mkArg i))
		       ) $ zip l [1..length l]
	mkArg i = "a" ++ show i

-- merge GILs with same guard
mergeIL :: [SH_GuardedInstrList] -> [SH_GuardedInstrList]
mergeIL l = 
    let l' = groupBy (\a b -> sameGuard a b) l
     in concatMap combGIL l'
  where sameGuard :: SH_GuardedInstrList -> SH_GuardedInstrList -> Bool
	sameGuard (SH_GuardedInstrList _ ga _ _) 
		  (SH_GuardedInstrList _ gb _ _) = ga == gb
	sameGuard _ _ = False
	combGIL :: [SH_GuardedInstrList] -> [SH_GuardedInstrList]
	combGIL [] = []
	combGIL l@((SH_GuardedInstrList _ g _ _):_) =
	    let h' = let l' = concatMapMaybes getHook l
		      in if l' == [] then Nothing else Just l' 
		l' = concatMap getIL l
	     in [SH_GuardedInstrList upos g h' l']
	getHook (SH_GuardedInstrList _ _ h _) = h
	getIL   (SH_GuardedInstrList _ _ _ l) = l
	concatMapMaybes :: (SH_GuardedInstrList -> Maybe [SH_HookCaller]) 
			-> [SH_GuardedInstrList]
			-> [SH_HookCaller]
	concatMapMaybes f l = concatMap (\e -> case f e of
					     Nothing -> []
				 	     Just hl -> hl) 
			                l

{-
-- FIXME kramer@acm.org reto -- prim_EXTEND BUG
- BUG: prim_EXTEND broken, guards are same, should be 1 only (merged)
    | guardE1 ->  change a = TRUE
                  change b = a
    | guardE1 ->  change a = TRUE
                  change b = (~a)
       !!! careful, double check prim_EXTEND3 still works if fixing this.   
-}
hackDeDup :: SH_FL_Spec -> SH_FL_Spec
hackDeDup spec = everywhere (mkT f) spec
    where f l@((SH_GuardedInstrList _ _ _ _):_) = remDupHACK l
	  f x = x

remDupHACK :: [SH_GuardedInstrList] ->  [SH_GuardedInstrList]
remDupHACK l = nub $ map remDupHACK0 (mergeIL (wipePos l))
  where remDupHACK0 :: SH_GuardedInstrList -> SH_GuardedInstrList
	remDupHACK0 (SH_GuardedInstrList _ g h l) =
	    SH_GuardedInstrList upos g h $ nub l

wipePos :: [SH_GuardedInstrList] ->  [SH_GuardedInstrList]
wipePos l = everywhere (mkT f) l
  where f x | Typeable.typeOf x == 
              Typeable.typeOf upos = -- i.e. is it a SourcePos?
                upos -- make them all the same
        f x | otherwise = x

---- HELPER -------------------------------------------------------------------
mk_AS_Ident s = AS_Ident epos [] s

mkPos :: String -> Int -> Int -> SourcePos
mkPos name line col = newPos name line col

upos = mkPos "foo" 0 0 
epos = (upos, Nothing, Nothing)
