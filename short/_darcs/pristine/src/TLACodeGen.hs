module TLACodeGen(groupSendInstr,
		  gen,
		  -- used for convenience in RewriteTimer
		  mk_AS_Type, combineInfix, mkVar, substSH_Instr,
		  typeKernel, mkView, subst, Pattern,
		  xify, allSingleMsgHandlerNames
		 ) where

import Debug.Trace(trace)
import Char (toLower, toUpper)
import Data.List (nub, (\\), intersperse, insertBy, foldl')
import Data.Maybe (fromJust)
import Data.Generics hiding (GT) -- avoid clash with Prelude
import Text.Regex
import Text.ParserCombinators.Parsec.Pos as PPos
import Language.TLAPlus.Syntax
import Language.TLAPlus.Pretty (prettyPrintE)
import Syntax
import Flatten
import ParserHelper(inlineOperatorDef)
import Rewrite(roleNames)

-- Data.HashTable(hash) is a poor hash function that lead to collisions
-- in the context of making msg handlers that differ slightly only in their
-- when clause.
import Codec.Binary.Base64 as Base64 -- crypto library, BSD
import Data.Digest.SHA1 as SHA1      -- crypto library, BSD

-- FIXME uses of ctx are indicative of a need to do processing in a reader
-- monad, so all function that need it can just 'get' the state w/o having
-- to visibly pass it around. Best to then make Ctx a record, so I can say
--   foo ctx and don't have to change all places if a new field is added.
type Ctx = ([SH_MsgDecl], [Role])

gen :: SH_FL_Spec -> String -> AS_Spec
gen spec name =
    let ctx = (msgDecl spec, roleDecl spec)
	st = extractTLAStateDeclList (msgDecl spec) (roleDecl spec)
	singleMsgHNames = allSingleMsgHandlerNamesAsConstOp spec
	c = (map (\s -> TLA_Constant s) $ constant spec ++ singleMsgHNames) ++
	    filter (\s -> case s of (TLA_Constant _) -> True 
		                    otherwise -> False) st
	v = filter (\s -> case s of (TLA_Variable _) -> True 
		                    otherwise -> False) st
        m = extractTLAMsgTypeList $ msgDecl spec -- contains msg and next els.
	verbtla = extractTLAVerbTLAList $ verbTLA spec
	-- FIXME kramer@acm.org reto -- horrible hack how lists are filtered
				     -- in TLA0, 1, 2
        l = concat [[mk_AS_Separator],
		    genTLA0 ( multiMessageRolePairs $ msgDecl spec)
		            ( listRoleNames $ roleDecl spec )  
		            ( c ++
			      v ++
			      verbtla ),
		    genTLA1 ( multiMessageRolePairs $ msgDecl spec)
		            ( listRoleNames $ roleDecl spec )  
			    ( m ++ st),
		    genTLA2 $ (extractTLAActionList ctx (roleDecl spec)) ++
		              (extractTLANextList $ roleDecl spec) ++ 
		              m ++
		              (extractTLASpecList $ roleDecl spec)]
     in wrapSpec name stdmodules l
    where wrapSpec name extends units = 
	      AS_Spec { name = name, 
			extendDecl = wrapExtends extends, 
			unitDef = units}
	  wrapExtends l = AS_ExtendDecl upos l
	  stdmodules = ["TLC", "Naturals", "FiniteSets", "Sequences"]

extractTLAStateDeclList :: [SH_MsgDecl] -> [Role] -> [TLA_GrpElement]
extractTLAStateDeclList msgDecl l = 
    concat $ map (extractTLAStateDecl msgDecl) l
 
extractTLAStateDecl :: [SH_MsgDecl] -> Role -> [TLA_GrpElement]
extractTLAStateDecl msgDecl r@(SH_RoleDef _info role _args l) =
    let var = mkVar role
	ty = role++"State"
	rs = if role == globalRole then [] else [role]
	l = [[TLA_Variable var],
	     map (\s -> TLA_Constant s) (rs ++ (viewStateTypes r)),
	     [TLA_AS_Wrapper 
	       (AS_OperatorDef upos 
		(AS_OpHead (mk_AS_Ident ty) []) 
		(AS_RecordType epos $ map mk_AS_RecordTypeElement 
		 (vardecl $ stateRec msgDecl r)))],
	     [if role == globalRole
	      then TLA_TypeInv (SH_Ty_UserDef upos ty, var)
	      else TLA_TypeInv (SH_Ty_Map upos 
				(SH_Ty_UserDef upos role) 
				(SH_Ty_UserDef upos ty), var)],
	     [TLA_Init var (initdecl role $ stateRec msgDecl r)] 
	    ] in 
	if hasState msgDecl r then concat l else []

vardecl :: [(SH_VarDecl, Maybe SH_ExprWrapper)] -> [SH_VarDecl]
vardecl l = map ( \(a,_) -> a) l

initdecl :: String -> [(SH_VarDecl, Maybe SH_ExprWrapper)] -> SH_ExprWrapper
initdecl role l = 
    if role == globalRole
    then SH_ExprWrapper upos (AS_RecordFunction epos (concat $ map mapsTo l))
    else SH_ExprWrapper upos 
		  (AS_QuantifierBoundFunction epos 
		   [AS_QBoundN [mk_AS_Ident $ lower role] 
		    (mk_AS_Ident role)] 
		   (AS_RecordFunction epos (concat $ map mapsTo l)))
  where mapsTo ((ty,i), Just (SH_ExprWrapper _ init)) =
	    [AS_MapTo (AS_Field i) init]
	mapsTo ((ty,i), Nothing) = []

stateRec :: [SH_MsgDecl] -> Role -> [(SH_VarDecl, Maybe SH_ExprWrapper)]
stateRec msgDecl (SH_RoleDef _info role _args l) =
    let statevar = map (\(SH_State _ _ vardecl init) -> (vardecl, init)) 
		       (filter isStateDecl l)
        viewvar = map (\(SH_ViewState _ ty init) -> 
		           let [k] = typeKernel ty in
		           ( (ty, mkView k),
		             Just init)) -- FIXME rewrite init 
		      (filter isViewStateDecl l)
	q = [((SH_Ty_SeqOf upos (SH_Ty_UserDef upos "Msg"), "g_inbox"), 
	    Just $ SH_ExprWrapper upos (AS_Tuple epos []))] -- = <<>>
	r = [( (SH_Ty_SeqOf upos
                 (SH_Ty_PairOf upos 
	           (SH_Ty_UserDef upos "Msg")
                   (SH_Ty_Expr upos 
		      (combineInfix AS_Cup $ 
		         map (AS_PrefixOP epos AS_SUBSET . mk_AS_Ident) 
		             (destRoles msgDecl role)))),
	       "g_obuf"),
	       Just $ SH_ExprWrapper upos (AS_Tuple epos [])
	     )] -- = <<>>
     in if hasMsgHandler l 
	then statevar ++ viewvar ++ q ++ (if doesMultiSend l then r else [])
	else statevar ++ viewvar      ++ (if doesMultiSend l then r else [])

viewStateTypes :: Role -> [String]
viewStateTypes  (SH_RoleDef _info role _args l) = 
    concat $ map ( \(SH_ViewState _ ty _init) -> typeKernel ty )
                 (filter isViewStateDecl l)

typeKernel (SH_Ty_UserDef _ s) = [s]
typeKernel (SH_Ty_UserDefOrNIL _ t) = typeKernel t
typeKernel (SH_Ty_Expr _ t) = ["anSH_Ty_Expr"]
typeKernel (SH_Ty_SetOf _ t) = typeKernel t
typeKernel (SH_Ty_SeqOf _ t) = typeKernel t
typeKernel (SH_Ty_PairOf _ tA tB) = (typeKernel tA) ++ (typeKernel tB)
typeKernel (SH_Ty_Map _ tA tB) = (typeKernel tA) ++ (typeKernel tB)
typeKernel (SH_Ty_Enum _ l) = l

hasState :: [SH_MsgDecl] -> Role -> Bool
hasState msgDecl r = stateRec msgDecl r /= []

isStateDecl (SH_State _ _ vardecl init) = True
isStateDecl _ = False
isViewStateDecl (SH_ViewState _ ty init) = True 
isViewStateDecl _ = False
isTimerStateDecl (SH_Timer _ _ id) = True
isTimerStateDecl _ = False

extractTLAMsgTypeList :: [SH_MsgDecl] -> [TLA_GrpElement]
extractTLAMsgTypeList l = concat $ map extractTLAMsgType l

-- Note: this generates not only the definition for the msg type, but also the
-- entry in the "Next ==" step operation to invoke the action that transfers
-- this type of message.
extractTLAMsgType :: SH_MsgDecl -> [TLA_GrpElement]
extractTLAMsgType m@(SH_MsgDecl anypos fromTy toTy msgty fields) =
  [TLA_MsgTypeDecl $ [(SH_Ty_Enum anypos [msgty], "type"), -- wrap ""
		      (fromTy, "sender")] ++ fields] ++
  (if isMultiDestMsg m -- NOTE: gens Next element
   then let a = head $ typeKernel fromTy 
            b = head $ typeKernel toTy in
            [TLA_Next False [(a, lower a)] 
                (mkDeliverMsgAction a b)]
   else [])

extractTLAVerbTLAList :: [VerbTLA] -> [TLA_GrpElement]
extractTLAVerbTLAList l = concat $ map extractTLAVerbTLA (defUseOrderTLA l)

defUseOrderTLA :: [VerbTLA] -> [VerbTLA]
defUseOrderTLA l = foldl' g [] l
  where g l x = insertBy defUse x l
        defUse a b | a == b = EQ
	defUse (SH_VerbTLAOp _ _ _ a) (SH_VerbTLAOp _ _ _ b) =
	  case a of
            (AS_OperatorDef _ (AS_OpHead (AS_Ident _ [] name) _) _) ->
	       if name `usedIn` b then LT else GT
            (AS_FunctionDef _ (AS_Ident _ [] name) _ _) ->
	       if name `usedIn` b then LT else GT
	    otherwise -> trace (" >> " ++ show a ++ " <<") GT
	defUse _ _ = GT
	usedIn :: String -> AS_UnitDef -> Bool
	usedIn name unit = 
	    [] /= (everything (++) ([] `mkQ` (f name unit))) unit
	f a u (AS_Ident _ [] b) = 
	    if a == b 
	    then [True] 
	    else []
	f _ _ _ = []

-- Note: this generates not only the definition for the msg type, but also the
-- entry in the "Next ==" step operation to invoke the action that transfers
-- this type of message.
-- By this time, the override is not relevant anymore, it has been resolved
-- in a previous rewrite step.
extractTLAVerbTLA :: VerbTLA -> [TLA_GrpElement]
extractTLAVerbTLA m@(SH_VerbTLAOp _pos _int _override unit) = 
    [TLA_AS_Wrapper unit]

multiMessageRolePairs :: [SH_MsgDecl] -> [(String, String)]
multiMessageRolePairs l = map pair (filter hasSetDest l)
  where hasSetDest (SH_MsgDecl _ _ toTy _ _) = isTypeSet toTy
	pair (SH_MsgDecl _ fromTy toTy _ _) = (head $ typeKernel fromTy, 
					       head $ typeKernel toTy)

extractTLAActionList :: Ctx -> [Role] -> [TLA_GrpElement]
extractTLAActionList ctx l = concat $ map (extractTLAAction ctx) l

extractTLAAction :: Ctx -> Role -> [TLA_GrpElement]
extractTLAAction ctx (SH_RoleDef _info role _args l) =
    concat $ map (extractTLAAction0 ctx) (filter isHandler l)
  where extractTLAAction0 ctx (SH_CallHandler _ _ when label arglist _ instr) =
	   [TLA_AS_Wrapper $
	      mkCallHandler ctx (protectSym l) role when label arglist instr]
	extractTLAAction0 ctx (SH_MsgHandler _ ann _ when msgtype 
			       label any from instr) =
	   [TLA_AS_Wrapper $
	      mkMsgHandler ctx ann (protectSym l) role when msgtype 
	                       label any from instr]
	extractTLAAction0 _ _ = []

protectSym :: [SH_RoleElement] -> [String]
protectSym l = concat $ map protectSym0 l
  where protectSym0 :: SH_RoleElement -> [String]
	protectSym0 (SH_State _ _ (_t, id) _) = [id]
	protectSym0 (SH_Timer _ _ id) = [id]
	protectSym0 _ = []

---- deal with TLC's limitation of not allowing Operation forward references
-- X prefix operation to deal with foward references
-- http://research.microsoft.com/users/lamport/tla/PlusCal.tla
xify s = "ZzZ" ++ s

-- FIXME kramer@acm.org reto -- need a better name!
msgH :: Bool -> Ctx -> (Maybe String) -> String 
     -> [(String, [(String, String)])]
msgH inclMulti ctx mtype rolename = 
    let (_, roleDecls) = ctx
	[role] = filter (\(SH_RoleDef _ r _ _) -> r == rolename) roleDecls
	(SH_RoleDef _ _ _ l) = role
     in concat $ map (f inclMulti mtype rolename) l
  where f inclMulti mtype name 
	  (SH_MsgHandler _ _ann _ when msgtype _label _any from 
	                 gillist) 
	    | from == Nothing = -- single message handler
	        let r = [(mkActionName msgtype when gillist, 
			  [(name, lower name), 
			   ("_dummyNatType", "msgpos")])]
		 in case mtype of
		      Nothing ->
		        [(mkActionName msgtype when gillist, 
			  [(name, lower name), 
			   ("_dummyNatType", "msgpos")])]
		      (Just mt) ->
		        if mt == msgtype then r else [] 
	    | from /= Nothing && inclMulti =
	        let r = [(mkActionName msgtype when gillist, 
			  [(name, lower name), 
			   ("_dummyNatType", "msgpos")])]
		 in case mtype of
		      Nothing ->
		        [(mkActionName msgtype when gillist, 
			  [(name, lower name), 
			   ("_dummyNatType", "msgpos")])]
		      (Just mt) ->
		        if mt == msgtype then r else []  
	f _ _ _ _ = []
msgH _ _ _ _ = []

allSingleMsgHandlerNames :: SH_FL_Spec -> [String]
allSingleMsgHandlerNames spec =
    let roles = roleDecl spec
	msgs = msgDecl spec
        ctx = (msgs, roles)
	rNames = roleNames roles 
        hs = concat $ map (msgH True ctx Nothing) rNames 
     in map (\(actionName, _) -> actionName) hs

allSingleMsgHandlerNamesAsConstOp :: SH_FL_Spec -> [String]
allSingleMsgHandlerNamesAsConstOp spec =
    let roles = roleDecl spec
	msgs = msgDecl spec
        ctx = (msgs, roles)
	rNames = roleNames roles 
        hs = concat $ map (msgH True ctx Nothing) rNames 
	-- FIXME kramer@acm.org reto -- it's a hack, how we put the const
				     -- together, but TLA_Constant takes a 
				     -- string only at the moment (not an expr)
     in map (\(actionName, l) -> 
	        actionName ++ 
	        "(" ++ (intersperse ',' (replicate (length l) '_')) ++ ")"
	    ) hs
---- deal with TLC's limitation of not allowing Operation forward references

mkCallHandler ctx protect role when label arglist gillist =
    let rs = if role == globalRole then [] else [(role, lower role)]
     in AS_OperatorDef upos 
          (AS_OpHead 
             (mk_AS_Ident $ mkActionName label when gillist) 
             (map ( \ (_,b) -> mk_AS_Ident b) (rs++arglist)))
          (AS_LAND epos $ ((mkWhen protect role when) ++ 
		           [mkCase ctx protect role gillist []]))

-- single destination case
-- X prefix operation to deal with foward references
-- http://research.microsoft.com/users/lamport/tla/PlusCal.tla
mkMsgHandler ctx ann protect role when msgtype label any from gillist 
    | from == Nothing =
  let inbox = AS_InfixOP epos AS_DOT 
                  (AS_InfixOP epos AS_FunApp 
		     (mk_AS_Ident $ mkVar role)
		     (AS_FunArgList epos [mk_AS_Ident $ lower role]))
	          (mk_AS_Ident "g_inbox")
      chkType = [(AS_InfixOP epos AS_EQ
		  (AS_InfixOP epos AS_DOT 
		   (AS_OpApp epos (mk_AS_Ident "Head") [inbox])
		   (mk_AS_Ident "type"))
		  (mk_AS_Ident $ show msgtype))]
      msgpos_e_sr = AS_OpApp epos (mk_AS_Ident "MsgPos") 
		     [inbox, 
		      AS_StringLiteral epos msgtype,
		      mk_AS_Ident "Local_F",
		      mk_AS_Ident $ lower role]
      -- IF Head(inbox).type = msgtype THEN 1 ELSE 0
      msgpos_e_reg = AS_IF epos 
		       (AS_InfixOP epos AS_EQ
		         (AS_InfixOP epos AS_DOT
		           (AS_OpApp epos (mk_AS_Ident "Head") [inbox])
		           (mk_AS_Ident "type"))
		         (AS_StringLiteral epos msgtype))
		       (AS_Num epos 1) -- head matches type
		       (AS_Num epos 0) -- no match
      msgpos_e = if annSelectiveReceive ann then msgpos_e_sr else msgpos_e_reg
      inbox_p = AS_InfixOP epos AS_FunApp 
		  inbox
		  (AS_FunArgList epos [mk_AS_Ident "local_p"])
      inbox_DropPos = AS_OpApp epos (mk_AS_Ident "DropPos") 
		        [inbox, mk_AS_Ident "local_p"]
      core_e = (mkCase ctx protect role gillist 
		  [TLA_I_Change 
		   role [mk_AS_Ident $ lower role] "g_inbox" inbox_DropPos])
      core_e' = defuseAssert core_e
      precond' = case core_e' of
                   (AS_Case _ pcond _) ->
		       let l = map (\(AS_CaseArm _ cond _) -> cond) pcond
                        in AS_LOR epos l
		   e ->
		       -- FIXME kramer@acm.org reto -- this introduces a lot
						    -- of TRUE /\ TRUE /\ ...
						    -- simplify expr!!
		       dropAssertPrimeUnchanged e -- a big LAND, but no case
      precondOp = AS_OperatorDef upos 
		    (AS_OpHead 
		      (mk_AS_Ident "Local_F") 
		      [mk_AS_Ident $ local(lower role), mk_AS_Ident msgtype])
		    (substAS_Expr [(lower role, local $ lower role)] precond')
      actions = filter 
		  (\(name, _) -> name /= mkActionName msgtype when gillist)
		  (msgH True {- incl. multi-handlers -} ctx Nothing role) 
      enabled_e_sr = AS_Quantified epos AS_All
		       [AS_QBoundN 
		         [mk_AS_Ident "local_i"] 
		           (AS_InfixOP epos AS_DOTDOT 
			     (AS_Num epos 1) 
			     (AS_InfixOP epos AS_Minus 
			       (mk_AS_Ident "local_p")
			       (AS_Num epos 1)))]
		    (if actions == []
		     then AS_Bool epos True
		     else (AS_LAND epos $ map mkEnabled actions))
      enabled_e_reg = AS_Bool epos True
      enabled_e = if annSelectiveReceive ann 
		  then enabled_e_sr 
		  else enabled_e_reg
   in AS_OperatorDef upos 
        (AS_OpHead
         (mk_AS_Ident $ xify (mkActionName msgtype when gillist))
         (map ( \ (_,b) -> mk_AS_Ident b) ([(role, lower role), 
					    ("_dummyNatType", "msgpos")])))
	(AS_LAND epos $ ((mkWhen protect role when) ++ (concat  
			 [[(AS_InfixOP epos AS_GT
			    (AS_OpApp epos (mk_AS_Ident "Len") [inbox])
			    (AS_Num epos 0))],
			  [AS_Let epos 
			     ((bindAllMsgSetRef [msgtype] 
			       inbox gillist Nothing) ++
			     [precondOp] ++
			     [AS_OperatorDef upos 
			      (AS_OpHead (mk_AS_Ident "local_p") []) msgpos_e])
			      -- under LET m == .. scope 
			      (AS_LAND epos [
			         (AS_InfixOP epos AS_GT 
				    (mk_AS_Ident "local_p") 
				    (AS_Num epos 0) ),
				 (AS_IF epos 
				   (AS_InfixOP epos AS_EQ 
				     (mk_AS_Ident "msgpos") 
				     (AS_Num epos 0))
				   enabled_e
				   (AS_InfixOP epos AS_EQ 
				    (mk_AS_Ident "local_p") 
				    (mk_AS_Ident "msgpos"))),
				 (AS_Let epos 
				    [AS_OperatorDef upos 
				       (AS_OpHead (mk_AS_Ident msgtype) []) 
				       inbox_p]
				    core_e')
				 ])]])))
  where mkEnabled :: (String, [(String, String)]) -> AS_Expression
	mkEnabled (actionName, argl) =
	     -- last argl element is "msgpos", drop and use local_i
             AS_PrefixOP epos AS_Not
		    (AS_OpApp epos (mk_AS_Ident "ENABLED") 
		      [AS_OpApp epos (mk_AS_Ident actionName) 
	                [mk_AS_Ident $ lower role, 
			 mk_AS_Ident "local_i"]])
	local s = "local_" ++ s
	-- when an action is "invoked" inside an ENABLE check, the
	-- assertion it might contain will fire. Disable this by
	-- or-ing (msgpos # 0) with the assertion expression
	defuseAssert :: AS_Expression -> AS_Expression
	defuseAssert e = everywhere (mkT f) e
	  where f a@(AS_OpApp _ (AS_Ident _ _ "Assert") (cond:rest)) =
		      AS_OpApp epos (mk_AS_Ident "Assert") 
				 ((AS_LOR epos 
				   [AS_InfixOP epos AS_NEQ
				      (mk_AS_Ident "msgpos") 
				      (AS_Num epos 0),
				    cond]
				  ):rest)
		f x = x
	dropAssertPrimeUnchanged e = everywhere (mkT f) e
	  where f (AS_InfixOP _ AS_EQ (AS_PostfixOP _ AS_Prime _) _) =
		    AS_Bool epos True
		f (AS_PrefixOP _ AS_UNCHANGED _) = 
		    AS_Bool epos True
		f (AS_OpApp _ (AS_Ident _ _ "Assert") _) =
		    AS_Bool epos True
		f x = x


-- all/majority case
mkMsgHandler ctx ann protect role when msgtype label _any (Just from) gillist =
  let inbox = AS_InfixOP epos AS_DOT 
                  (AS_InfixOP epos AS_FunApp 
		     (mk_AS_Ident $ mkVar role)
		     (AS_FunArgList epos [mk_AS_Ident $ lower role]))
	          (mk_AS_Ident "g_inbox") 
      senderType = case from of 
		     (SH_FromAll s, _w) -> s
		     (SH_FromMaj s, _w) -> s
		     (SH_FromExp s _e, _w) -> s
      view = AS_InfixOP epos AS_DOT 
                  (AS_InfixOP epos AS_FunApp 
		     (mk_AS_Ident $ mkVar role)
		     (AS_FunArgList epos [mk_AS_Ident $ lower role]))
	          (mk_AS_Ident $ mkView senderType)
      setPred = case from of
		(SH_FromAll _, _) -> 
		    (AS_LAND epos
		      [AS_InfixOP epos AS_EQ -- ALL
		         (mk_AS_Ident "sender_set") 
		         view,
		       AS_InfixOP epos AS_NEQ -- don't trigger on empty view {}
		         (AS_DiscreteSet epos []) 
		         view])
		(SH_FromExp _t (SH_ExprWrapper _ expr), _) -> 
		    (AS_InfixOP epos AS_EQ -- ALL
		       (mk_AS_Ident "sender_set") 
		       (rewriteExpr protect role expr)) -- custom expr
		(SH_FromMaj _, _) ->
		    AS_LAND epos
		      [AS_OpApp epos 
		        (mk_AS_Ident "Majority") 
		         [mk_AS_Ident "sender_set",
			  view], -- MAJORITY
		       AS_InfixOP epos AS_NEQ -- don't trigger on empty view {}
		         (AS_DiscreteSet epos []) 
		         view]
      wherePred = case from of
		    (_, Just (SH_ExprWrapper _ where_expr, quant)) -> 
			let kernel = substAS_Expr 
				       [(msgtype, "temp_m")] 
				       (rewriteExpr protect role where_expr)
		            e = AS_Quantified epos 
			          (case quant of
			             SH_All  -> AS_All
			             SH_Some -> AS_Exist
			             SH_None -> AS_All)
				  [AS_QBoundN
			            [mk_AS_Ident "temp_m"]
			            (mk_AS_Ident msgtype)]
				  kernel
		         in [e]
		    _ ->
			[]
      -- FIXME kramer@acm.org reto -- should I include multi handlers? 
      actions = filter 
		  (\(name, _) -> name /= mkActionName msgtype when gillist)
		  (msgH False ctx Nothing {- match all msg types -} role) 
      enabled_e = AS_Quantified epos AS_All
		    [AS_QBoundN 
		       [mk_AS_Ident "local_i"] 
		       (AS_InfixOP epos AS_DOTDOT 
			  (AS_Num epos 1) 
		          (mk_AS_Ident "maxi"))]
		    (if actions == []
		     then AS_Bool epos True
		     else (AS_LAND epos $ map mkEnabled actions))
      regular_e = [enabled_e] ++ -- check that no single m hndlr is
	           wherePred ++ [
		  (mkCase ctx protect role gillist 
		   [TLA_I_Change 
		    role [mk_AS_Ident $ lower role] "g_inbox"
		    (AS_OpApp epos (mk_AS_Ident "SelectSeq") 
		     [inbox, 
		      mk_AS_Ident "KeepMsgTest"])])
		  ]
      regular_e' = AS_IF epos 
	             (AS_InfixOP epos AS_EQ
	               (mk_AS_Ident "msgpos")
		       (AS_Num epos 0))
		     (AS_LAND epos regular_e)
		     (AS_InfixOP epos AS_AND
		       (AS_InfixOP epos AS_NEQ 
			 (mk_AS_Ident "maxi") 
			 (AS_Num epos 0))
		       (AS_InfixOP epos AS_LTEQ 
		         (mk_AS_Ident "maxi")
		         (mk_AS_Ident "msgpos")))
   in AS_OperatorDef upos 
        (AS_OpHead 
         (mk_AS_Ident $ xify (mkActionName msgtype when gillist)) 
         (map ( \ (_,b) -> mk_AS_Ident b) ([(role, lower role),
					    ("_dummyNatType", "msgpos")])))
	(AS_LAND epos $ ((mkWhen protect role when) ++ 
			 [(AS_InfixOP epos AS_GT
			   (AS_OpApp epos (mk_AS_Ident "Len") [inbox])
			   (AS_Num epos 0)),
			  (AS_Let epos 
			   ((bindAllMsgSetRef [msgtype] 
			       inbox gillist (Just view)) ++
			    (mkMTypeLet msgtype inbox (Just view)) ++
			    [AS_OperatorDef upos 
			      (AS_OpHead (mk_AS_Ident "sender_set") [])
			      (AS_SetGeneration epos
				 (AS_InfixOP epos AS_DOT
				   (mk_AS_Ident "m") 
				   (mk_AS_Ident "sender")) 
                                 (AS_QBound1 
				   (mk_AS_Ident "m") 
				   (mk_AS_Ident msgtype))),
			     AS_OperatorDef upos 
			      (AS_OpHead (mk_AS_Ident "KeepMsgTest") 
			         [mk_AS_Ident "m"])
				 (AS_InfixOP epos AS_NotIn
				   (mk_AS_Ident "m") 
				   (mk_AS_Ident msgtype))])
			   -- under LET m == .. scope 
			   (AS_LAND epos (
			    [setPred] ++
			    [regular_e'])))
			 ]))
  where mkEnabled :: (String, [(String, String)]) -> AS_Expression
	mkEnabled (actionName, argl) =
	     -- last argl element is "msgpos", drop and use local_i
             AS_PrefixOP epos AS_Not
		    (AS_OpApp epos (mk_AS_Ident "ENABLED") 
		      [AS_OpApp epos (mk_AS_Ident actionName) 
	                [mk_AS_Ident $ lower role, 
			 mk_AS_Ident "local_i"]])
{-
  /\ (LET res ==
            {m \in Msg: \E i \in DOMAIN((st_Client[client]).g_inbox): /\ (m = (st_Client[client]).g_inbox[i]) /\ (m.type = "res")} 

          is == { i \in DOMAIN((st_Client[client]).g_inbox):
                    ((st_Client[client]).g_inbox[i]).type = "res" }
          maxi = CHOOSE i \in is: 
		  \A j \in js: j <= i
          res = { (st_Client[client]).g_inbox[i]: i \in is }
-}
mkMTypeLet msgtype inbox view =
    let is = AS_OperatorDef upos
	       (AS_OpHead (mk_AS_Ident "is") [])
	       (AS_SetComprehension epos
		  (AS_QBound1 
		     (mk_AS_Ident "local_i") 
		     (AS_OpApp epos 
		        (mk_AS_Ident "DOMAIN") 
		        [inbox]))
		  (AS_LAND epos
		     ([AS_InfixOP epos AS_EQ
		        (AS_InfixOP epos AS_DOT
  			  (AS_InfixOP epos AS_FunApp 
		            inbox 
		            (AS_FunArgList epos [mk_AS_Ident "local_i"]))
			  (mk_AS_Ident "type"))
		        (mk_AS_Ident $ show msgtype)] ++
		      (case view of
		        Nothing -> []
		        (Just view) ->
		           [AS_InfixOP epos AS_In
		             (AS_InfixOP epos AS_DOT
  			        (AS_InfixOP epos AS_FunApp 
				  inbox 
				  (AS_FunArgList epos [mk_AS_Ident "local_i"]))
			        (mk_AS_Ident "sender"))
		             view]))))
	maxi = AS_OperatorDef upos
	         (AS_OpHead (mk_AS_Ident "maxi") [])
		 (AS_IF epos 
		   (AS_InfixOP epos AS_NEQ 
		      (mk_AS_Ident "is") 
		      (AS_DiscreteSet epos []))
    		   (AS_Choose epos 
		      (AS_QBound1 
		       (mk_AS_Ident "local_i") 
		       (mk_AS_Ident "is"))
		      (AS_Quantified epos AS_All
		       [AS_QBoundN [mk_AS_Ident "local_j"] (mk_AS_Ident "is")]
		       (AS_InfixOP epos AS_LTEQ
			(mk_AS_Ident "local_j")
			(mk_AS_Ident "local_i"))))
		   (AS_Num epos 0))
	msgs = AS_OperatorDef upos 
	         (AS_OpHead (mk_AS_Ident msgtype) [])
		 (AS_SetGeneration epos
  		   (AS_InfixOP epos AS_FunApp 
		      inbox 
		      (AS_FunArgList epos [mk_AS_Ident "local_i"]))
		   (AS_QBound1 (mk_AS_Ident "local_i") 
		     (mk_AS_Ident "is")))
     in [is, maxi, msgs]

findAllMsgSetRef :: [String] -> [SH_GuardedInstrList] -> [String]
findAllMsgSetRef cov gil = (nub $ (everything (++) ([] `mkQ` f)) gil) \\ cov
  where f (AS_OpApp _ (AS_Ident _ _ f) [AS_Ident _ _ mtype])
	    | elem (lower f) ["all", "any", "any2", "senders"] = [mtype]
	    | otherwise = []
	f x = []

-- for each ALL/ANY/SENDERS mtype found inside
bindAllMsgSetRef cov inbox gil view = 
  let mtypes = findAllMsgSetRef cov gil
   in concat $ map (f inbox view) mtypes
  where f inbox view msgtype = mkMTypeLet msgtype inbox view

mkCase :: Ctx -> [String] -> String -> [SH_GuardedInstrList] -> [TLA_I_Change] 
       -> AS_Expression
mkCase ctx protect role gillist inheritedChgs =
    if (any hasCaseArm gillist)
       then let (gillist', otherArm) =
		  case gillist of
		    [one] -> (gillist, Nothing)
		    l -> let other = last l
			     butLast = (reverse . tail . reverse) l
			  in case other of
			       (SH_GuardedInstrList _
				    (Just (SH_ExprWrapper _ e)) _ _) ->
			          case e of
				    AS_Ident _ _ "otherwise" -> 
   				      (butLast, 
				       Just $ mkCaseArmOther 
				                ctx protect role inheritedChgs
				                other)
				    AS_LAND _ [AS_Ident _ _ "otherwise"] ->
   				      (butLast, 
				       Just $ mkCaseArmOther 
				                ctx protect role inheritedChgs
				                other)
				    _ -> (l, Nothing)
			       _ -> (l, Nothing)
             in AS_Case epos (map (mkCaseArm ctx protect role inheritedChgs) 
		 	          gillist') 
	                otherArm
       else let chgs = concat $ map (mkUnguardedInstr ctx protect role) gillist
	     in grpTLA_I_ChangeList ctx (chgs++inheritedChgs)

mkCaseArm ctx protect role inheritedChgs 
	  (SH_GuardedInstrList _ guard hooks instr) =
    let chgs = concat $ map (mkTLAInstr ctx protect role) instr
	is = grpTLA_I_ChangeList ctx (chgs ++ inheritedChgs)
	[g] = mkWhen protect role guard
     in AS_CaseArm epos g is

mkCaseArmOther ctx protect role inheritedChgs 
	  (SH_GuardedInstrList _ _ hooks instr) =
    let chgs = concat $ map (mkTLAInstr ctx protect role) instr
	is = grpTLA_I_ChangeList ctx (chgs ++ inheritedChgs)
     in AS_OtherCaseArm epos is

mkUnguardedInstr ctx protect role (SH_GuardedInstrList _ _ _ instr) =
    concat $ map (mkTLAInstr ctx protect role) instr

hasCaseArm (SH_GuardedInstrList _ (Just _) _ _) = True
hasCaseArm (SH_GuardedInstrList _ Nothing _ _) = False

mkWhen :: [String] -> String -> Maybe SH_ExprWrapper -> [AS_Expression]
mkWhen _ _ Nothing = []
mkWhen protect role (Just (SH_ExprWrapper _ e)) = [rewriteExpr protect role e]

-----

groupSendInstr :: SH_FL_Spec -> SH_FL_Spec 
groupSendInstr spec = everywhere (mkT f) spec
  where f (SH_GuardedInstrList info guard label l) =
	    SH_GuardedInstrList info guard label (groupSend l)
groupSend :: [SH_Instr] -> [SH_Instr]
groupSend l =
    let s = filter isSendInstr l
     in if s == []
	then l
	else (l \\ s) ++ [SH_I_SendGroup upos s]
  where isSendInstr (SH_I_MsgSend1 _ _ _ _ _ _ _) = True
	isSendInstr _ = False

---- INSTRUCTION CODE GENERATION ----------------------------------------------

mkTLAInstr :: Ctx -> [String] -> String -> SH_Instr -> [TLA_I_Change]
mkTLAInstr ctx protect role (SH_I_ChangeState _ ass) =
    concat $ map (mkExceptAssignment protect Nothing role) ass
mkTLAInstr ctx protect role (SH_I_ChangeView _ viewedRole
			      (SH_ExprWrapper _ e)) =
    concat $ map (mkExceptAssignment protect Nothing role) 
	       [SH_ExprWrapper upos 
		(AS_InfixOP epos AS_EQ 
		 (AS_Ident epos [] $ mkView viewedRole) 
		 e)]
mkTLAInstr ctx protect role (SH_I_SendGroup _ l) = 
  case l of
    [SH_I_MsgSend1 _ _ False last _ _ _] -> -- 1 single destination (!) only
       mkTLAInstr ctx protect role (head l)
    _ -> -- either we have multiple !, or !! or a mixture. In any case, wrap
         -- the single dest ones in a dest group (like !!) to maintain in 
         -- order delivery
       tlaSend ctx protect role l

-- NOTE kramer@acm.org reto -- MsgSend1 does not appear in the [Instr] list
-- since the groupSendInstr (called from short.hs) wrapped them into the 
-- group send. BUT it's used from inside the base SendGroup case.
-- FIXME kramer@acm.org reto -- restructure.
mkTLAInstr ctx protect role (SH_I_MsgSend1 _ _ multi last dest mtype pairs) = 
    let args' = map (\(s,SH_ExprWrapper _ e) -> 
		         -- view(X) handling
	                 let e' = rewriteExpr protect role e in 
			     AS_MapTo (AS_Field s) e') pairs
        msg = AS_RecordFunction epos 
	        ([AS_MapTo (AS_Field "type") (mk_AS_Ident $ show mtype), -- ""
	          AS_MapTo (AS_Field "sender") (mk_AS_Ident $ lower role) 
	         ] ++ args')
	destrole = case dest of
		     (SH_VIEW_REF _ s) -> s
		     _ -> let (SH_MsgDecl _ _ toRole _ _) = 
				  mkTLAInstrMsgDef ctx mtype 
                              [t] = typeKernel toRole
			   in t
        (navapp, field) = case dest of 
			    (SH_VIEW_REF _ _s) ->
			      ((AS_InfixOP epos AS_DOT 
				(AS_InfixOP epos AS_FunApp 
				 (mk_AS_Ident $ mkVar role) 
				 (AS_FunArgList epos [mk_AS_Ident $ 
						      lower role]))
				(mk_AS_Ident $ mkView destrole))
			      ,"g_inbox")
			    (SH_ExprWrapper _ e) -> 
				(e, "g_inbox")
	msg' = if multi 
	       then AS_Tuple epos [msg, navapp]
	       else msg
	newv = if last -- last message from sender (before crash?)
	       then AS_Tuple epos [msg']
	       else AS_OpApp epos (mk_AS_Ident "Append") [AS_OldVal, msg']
     in if multi
	then [TLA_I_Change role     [mk_AS_Ident $ lower role] "g_obuf" newv] 
	else [TLA_I_Change destrole [navapp]                   field  newv]
mkTLAInstr ctx protect role (SH_I_Assert _ (SH_ExprWrapper _ e) s l) = 
    let l' = case l of 
	   Nothing -> []
	   (Just x) -> x in
        [TLA_I_Assert role protect e s (map (\(SH_ExprWrapper _ x) -> x) l')]
mkTLAInstr ctx protect role (SH_I_Let _ bindings) =
    [TLA_I_Let (map (\(s, SH_ExprWrapper _ e) -> 
                        (s,rewriteExpr protect role e)) 
                    bindings)]
mkTLAInstr ctx protect role (SH_I_ForeignChangeState _ foreignrole var ass) =
    concat $ map (mkExceptAssignment protect var foreignrole) ass
mkTLAInstr ctx protect role (SH_I_FailTLAClause _) =
    [TLA_I_FailTLAClause]
mkTLAInstr ctx protect role (SH_I_Drop _ m) =
    [TLA_I_Drop m]
mkTLAInstr _ _ _ _ = []

--
tlaSend :: Ctx -> [String] -> String -> [SH_Instr] -> [TLA_I_Change]
tlaSend ctx protect role l =
  let hasLastGasp = any isLastGasp l
      els = map (mkQEntry ctx protect role) l
      newv = if length l == 1
	     then if hasLastGasp
		  then AS_Tuple epos els -- not els is singleton
	          else AS_OpApp epos (mk_AS_Ident "Append") [AS_OldVal, head els]
	     else if hasLastGasp
		  then AS_Tuple epos els -- reset queue!
		  else AS_InfixOP epos AS_Circ AS_OldVal (AS_Tuple epos els)
   in [TLA_I_Change role [mk_AS_Ident $ lower role] "g_obuf" newv]
  where 
    mkQEntry ctx protect role (SH_I_MsgSend1 _ _ multi last dest mtype pairs) =
      let args' = map (\(s,SH_ExprWrapper _ e) -> 
	  	          -- view(X) handling
		          let e' = rewriteExpr protect role e in 
			      AS_MapTo (AS_Field s) e') pairs
          msg = AS_RecordFunction epos 
	         ([AS_MapTo (AS_Field "type") (mk_AS_Ident $ show mtype), -- ""
	           AS_MapTo (AS_Field "sender") (mk_AS_Ident $ lower role) 
	         ] ++ args')
	  destrole = case dest of
		       (SH_VIEW_REF _ s) -> s
		       _ -> let (SH_MsgDecl _ _ toRole _ _) = 
			  	    mkTLAInstrMsgDef ctx mtype 
                                [t] = typeKernel toRole
			     in t
          navapp = case dest of 
		     (SH_VIEW_REF _ _s) ->
			 (AS_InfixOP epos AS_DOT 
			     (AS_InfixOP epos AS_FunApp 
			      (mk_AS_Ident $ mkVar role) 
			      (AS_FunArgList epos [mk_AS_Ident $ 
					  	   lower role]))
			   (mk_AS_Ident $ mkView destrole))
		     (SH_ExprWrapper _ e) -> 
			 if multi then e else AS_DiscreteSet epos [e]
       in AS_Tuple epos [msg, navapp]

isLastGasp :: SH_Instr -> Bool
isLastGasp (SH_I_MsgSend1 _ _ _ last _ _ _) = last
isLastGasp _ = False
--

mkTLAInstrMsgDef :: Ctx -> String -> SH_MsgDecl
mkTLAInstrMsgDef (l,_) ty = 
    head $ filter (\(SH_MsgDecl _ _ _ t _) -> t == ty) l

-- Straight case where assignment is to variable
mkExceptAssignment protect var role (SH_ExprWrapper _ (AS_InfixOP _ AS_EQ 
						       (AS_Ident _ _ s) e)) =
    let rs = if role == globalRole 
	     then []
	     else case var of
	            Nothing -> [mk_AS_Ident $ lower role]
		    (Just nav) -> [nav]
     in [TLA_I_Change role rs s (rewriteExpr protect role e) ]
-- FIXME kramer@acm.org reto -- ADD SUPPORT FOR "GLOBAL" and "var" here also!
-- CHANGE map[b] = TRUE case where variable itself is an array
-- see prim_assign1.short.
-- This is really convenience for CHANGE map = [@ EXCEPT ![b] = TRUE]
mkExceptAssignment protect var role (SH_ExprWrapper _ 
				     (AS_InfixOP _ AS_EQ 
				      (AS_InfixOP _ AS_FunApp 
				       (AS_Ident _ _ s) 
				       (AS_FunArgList _ idx)) e)) =
    [TLA_I_Change role [mk_AS_Ident $ lower role] s 
                  (rewriteExpr protect role 
		   (AS_Except AS_OldVal
		    [AS_ExceptAssignment 
		     [AS_ExceptNavApp idx]
		     e]))]
-- CHANGE foo[b,c].bar = ..., like above, but with .bar wrapped around
mkExceptAssignment protect var role (SH_ExprWrapper _ 
				     (AS_InfixOP _ AS_EQ
				      (AS_InfixOP _ AS_DOT
				       (AS_InfixOP _ AS_FunApp 
					(AS_Ident _ _ s) 
					(AS_FunArgList _ idx))
				       (AS_Ident _ _ field)) 
				      e)) =
  let e' = (AS_Except AS_OldVal
	    [AS_ExceptAssignment 
	     [AS_ExceptNavField $ AS_Field field]
	     e]) in
    [TLA_I_Change role [mk_AS_Ident $ lower role] s 
                  (rewriteExpr protect role 
		   (AS_Except AS_OldVal
		    [AS_ExceptAssignment 
		     [AS_ExceptNavApp idx]
		     e']))]

-- FIXME kramer@acm.org reto --  add support for (replication_core2)
-- change \A k \in c_owned_keys: owner[k] = Nil /\ locked[k] = FALSE
mkExceptAssignment _ _ _ _ = []

extractTLANextList :: [Role] -> [TLA_GrpElement]
extractTLANextList l = concat $ map extractTLANext l

extractTLANext :: Role -> [TLA_GrpElement]
extractTLANext (SH_RoleDef _info role _args l) =
    concat $ map (extractTLANext0 role) (filter isHandler l)
  where extractTLANext0 role (SH_CallHandler _ _ when label arglist _ instr) =
	    let rs = if role == globalRole then [] else [(role, lower role)] 
	     in [TLA_Next False (rs++arglist) (mkActionName label when instr)]
	extractTLANext0 role (SH_MsgHandler _ _ann _ 
			        when msgtype label any from instr) =
	     [TLA_Next True
	        [(role, lower role)] (mkActionName msgtype when instr)]
	extractTLANext0 _ _ = []

extractTLASpecList :: [Role] -> [TLA_GrpElement]
extractTLASpecList l = concat $ map (\r -> 
				     case roleName r of
				       Nothing -> []
				       Just n -> [TLA_Spec n]) l

{- FIXME move these to the Syntax module and add as queries -}
roleName :: Role -> Maybe String
roleName (SH_RoleDef _info role _args l) = Just role
roleName _ = Nothing

lower = map toLower

capFirst :: String -> String
capFirst [] = []
capFirst (h:rest) = toUpper h : rest

isMsgHandler (SH_MsgHandler _ _ _ _ _ _ _ _ _) = True
isMsgHandler _ = False

isHandler (SH_MsgHandler _ _ _ _ _ _ _ _ _) = True
isHandler (SH_CallHandler _ _ _ _ _ _ _) = True
isHandler _ = False

hasMsgHandler l = filter isMsgHandler l /= []

isMultiSend :: SH_Instr -> Bool
isMultiSend (SH_I_SendGroup _ l) = 
    [] /= (filter (\i -> case i of
		           (SH_I_MsgSend1 _ _ multi _ _ _ _) -> multi
		           _ -> False) 
	   l)
-- FIXME kramer@acm.org reto -- remove this send1
isMultiSend (SH_I_MsgSend1 _ _ multi _ _ _ _) = multi
isMultiSend _ = False

doesMultiSend :: [SH_RoleElement] -> Bool
doesMultiSend l = (filter isMultiSend (concat $ map listGInstr l)) /= []
  where listGInstr :: SH_RoleElement -> [SH_Instr]
        listGInstr (SH_MsgHandler _ _ _ _ _ _ _ _ l) = concat $ map listInstr l 
	listGInstr (SH_CallHandler _ _ _ _ _ _ l) = concat $ map listInstr l
	listGInstr (SH_TimeoutHandler _ _ _ _ _ l) = concat $ map listInstr l
	listGInstr (SH_CrashHandler _ _ _ _ _ _ _ l) = concat $ map listInstr l
	listGInstr (SH_Every _ _ _ _ _ l) = concat $ map listInstr l
	listGInstr (SH_Extend_Hook _ _ _ l) = concat $ map listInstr l
	listGInstr _ = []
	listInstr :: SH_GuardedInstrList -> [SH_Instr]
	listInstr (SH_GuardedInstrList _ _ _ l) = l

destRoles :: [SH_MsgDecl] -> String -> [String]
destRoles l sendingRole =
    let sm = filter (\(SH_MsgDecl _ s d _ _) -> 
	               typeKernel s == [sendingRole]) l
     in nub $ map (\(SH_MsgDecl _ _ d _ _) -> head $ typeKernel d) sm

rewriteExpr protect role = everywhere (mkT (f protect role))
  where f protect "GLOBAL" i = i -- do not wrap reference inside GLOBAL role
	f protect "global" i = i -- do not wrap reference inside GLOBAL role
        f protect role i@(AS_Ident _ _ s) | elem s protect = -- conditional rew
	    AS_InfixOP epos AS_DOT                           -- if to be prot.
                  (AS_InfixOP epos AS_FunApp 
		     (mk_AS_Ident $ mkVar role)
		     (AS_FunArgList epos [mk_AS_Ident $ lower role]))
	          i
	f protect role (AS_OpApp _ (AS_Ident _ _ v) -- unconditional rewr.
			[(AS_Ident _ _ viewedRole)])   -- to cover view(X)
	    | (v == "VIEW") || -- special operation 
	      (v == "view") = AS_InfixOP epos AS_DOT 
			      (AS_InfixOP epos AS_FunApp 
			       (mk_AS_Ident $ mkVar role)
			       (AS_FunArgList epos [mk_AS_Ident $ lower role]))
			      (mk_AS_Ident $ mkView viewedRole)
	f protect role i@(AS_Ident _ _ self) 
	    | (self == "SELF") || -- special variable 
	      (self == "self") = mk_AS_Ident $ lower role
	f _ _ x = x

-------------------------------------------------------------------------------
data TLA_I_Change = TLA_I_Change
		      String {- Role owning the changed state -}
		      [AS_Expression] {- nav app, [] for global var -}
		      String {- field name -}
		      AS_Expression {- new value -}
		  | TLA_I_Assert
		      String {- role, needed to protect bound expressions -}  
		      [String] {- protect -}
		      AS_Expression String [AS_Expression]
		  | TLA_I_FailTLAClause
		  | TLA_I_Let [(String, AS_Expression)]
		  | TLA_I_Drop String
		    deriving (Eq, Ord, Show)

grpTLA_I_ChangeList :: Ctx -> [TLA_I_Change] -> AS_Expression
grpTLA_I_ChangeList ctx changes =
  let completeRoleSet = allRoleNames ctx
      roles = roleNamesTLA_I_Change changes
      expr = AS_LAND epos 
	           ((grpTLA_I_DropList changes) ++
		    (grpTLA_I_FailList changes) ++
		    (grpTLA_I_AssertList changes) ++
		    (concat $ map (grpTLA_I_ChangeList0 ctx changes) roles)++ 
		    if (completeRoleSet \\ roles) == []
		    then [] -- all roles (global set) are changed
		    else [AS_PrefixOP epos AS_UNCHANGED $
			    AS_Tuple epos (map (mk_AS_Ident . mkVar) 
		                               (completeRoleSet \\ roles))]) 
      lets = grpTLA_I_Let changes
   in if lets == [] then expr else AS_Let epos lets expr
  where grpTLA_I_ChangeList0 ctx changes role =
	    let chgs = concat $ map (grpTLA_ExceptAss ctx role) changes in 
		if chgs == [] 
		   then []
		   else [AS_InfixOP epos AS_EQ
			   (AS_PostfixOP epos AS_Prime 
			      (mk_AS_Ident $ mkVar role))
			   (AS_Except (mk_AS_Ident $ mkVar role) chgs)]

grpTLA_I_Let :: [TLA_I_Change] -> [AS_UnitDef]
grpTLA_I_Let l = concat $ map grpTLA_I_Let0 l
  where grpTLA_I_Let0 :: TLA_I_Change -> [AS_UnitDef]
	grpTLA_I_Let0 (TLA_I_Let bindings) =
	    map (\(s, e) -> 
		  AS_OperatorDef upos (AS_OpHead (mk_AS_Ident s) []) e) 
	        bindings
	grpTLA_I_Let0 _ = []

grpTLA_ExceptAss :: Ctx -> String -> TLA_I_Change -> [AS_ExceptAssignment] 
grpTLA_ExceptAss ctx role (TLA_I_Change r nav field e) =
    if role /= r 
    then []
    else let nav' = if nav /= [] then [AS_ExceptNavApp nav] else [] -- GLOBAL role has no nav
          in [AS_ExceptAssignment (nav' ++ [AS_ExceptNavField (AS_Field field)]) e]
grpTLA_ExceptAss _ _ _ = []

grpTLA_I_AssertList :: [TLA_I_Change] -> [AS_Expression]
grpTLA_I_AssertList l = concat $ map grpTLA_I_Assert l

grpTLA_I_Assert (TLA_I_Assert role protect e s l) =
    [AS_OpApp epos 
     (mk_AS_Ident "Assert") 
     [rewriteExpr protect role e, 
      AS_Tuple epos ([mk_AS_Ident $ show s] ++  
		     (map (\e -> 
			   AS_Tuple epos [mk_AS_Ident (show $ prettyPrintE e), 
					  rewriteExpr protect role e]) l))]]
grpTLA_I_Assert _ = []

grpTLA_I_FailList :: [TLA_I_Change] -> [AS_Expression]
grpTLA_I_FailList l = concat $ map grpTLA_I_Fail l

grpTLA_I_Fail (TLA_I_FailTLAClause) = [mk_AS_Ident "FALSE"] 
grpTLA_I_Fail _ = []

grpTLA_I_DropList :: [TLA_I_Change] -> [AS_Expression]
grpTLA_I_DropList l = concat $ map grpTLA_I_Drop l

grpTLA_I_Drop (TLA_I_Drop _) = [mk_AS_Ident "TRUE"] -- enable inbox update 
grpTLA_I_Drop _ = []

roleNamesTLA_I_Change l = 
    nub $ concat $ map roleNamesTLA_I_Change0 l
  where roleNamesTLA_I_Change0 (TLA_I_Change role _ _ _) = [role]
	roleNamesTLA_I_Change0 _ = []

---- TLA ----------------------------------------------------------------------
data TLA_GrpElement = TLA_AS_Wrapper AS_UnitDef
		    | TLA_Constant String
		    | TLA_Variable String
		    | TLA_MsgTypeDecl [SH_VarDecl]
		    | TLA_TypeInv SH_VarDecl
		    | TLA_Init String SH_ExprWrapper
		    | TLA_Next
		        Bool {- is this a single-message handler -}
		        [(String, String)]
			String {- handlername -}
		    | TLA_Spec String
		    | TLA_Assert SH_ExprWrapper
		      deriving (Eq, Ord, Show)

-- User defined types
-- Msg == (may include references to user defined types)
-- Role Types (incl. reference to Msg for queues)
genTLA0 :: [(String, String)] -> [String] -> [TLA_GrpElement] -> [AS_UnitDef]
genTLA0 rolePairs role l = [genTLA_Constant l,
			    genTLA_Variable l,
			    mk_AS_Separator] ++
			   (genTLA_Boilerplate rolePairs role) ++
			   [mk_AS_Separator] ++
			   (genTLA_Wrapper l) -- user def types and operators

-- User defined types
-- Msg == (may include references to user defined types)
-- Role Types (incl. reference to Msg for queues)
genTLA1 :: [(String, String)] -> [String] -> [TLA_GrpElement] -> [AS_UnitDef]
genTLA1 rolePairs role l = [mk_AS_Separator] ++
			   (genTLA_MsgTypeDecl l) ++ -- Msg ==
			   [mk_AS_Separator] ++
			   (genTLA_Wrapper l) ++ -- xState = [...] records
			   [mk_AS_Separator,
			    genTLA_TypeInv l,
			    genTLA_Init l]

genTLA2 :: [TLA_GrpElement] -> [AS_UnitDef]
genTLA2 l = [mk_AS_Separator] ++
	    (genTLA_Wrapper l) ++ -- Action declarations
	    [mk_AS_Separator,
	     genTLA_Fairness l,
	     mk_AS_Separator,
	     genTLA_Next l,
	     mk_AS_Separator,
	     genTLA_Spec l]

genTLA_MsgTypeDecl :: [TLA_GrpElement] 
		   -> [AS_UnitDef] -- can be empty if there's no msg
genTLA_MsgTypeDecl l =
    let ml = concat $ map mkTLA_MsgTypeDeclEntry l
     in if ml == [] 
	then [] 
	else [AS_OperatorDef upos 
	      (AS_OpHead (mk_AS_Ident "Msg") [])
	      (combineInfix AS_Cup ml)]
  where mkTLA_MsgTypeDeclEntry (TLA_MsgTypeDecl l) =
	    [AS_RecordType epos $ map mk_AS_RecordTypeElement l]
	mkTLA_MsgTypeDeclEntry _ = []

genTLA_Constant :: [TLA_GrpElement] -> AS_UnitDef
genTLA_Constant l = AS_ConstantDecl upos $ nub (concat (map mkTLA_Constant l)) 
  where mkTLA_Constant (TLA_Constant s) = [mk_AS_Ident s]
	mkTLA_Constant _ = []

genTLA_Variable :: [TLA_GrpElement] -> AS_UnitDef
genTLA_Variable l = AS_VariableDecl upos $ nub (concat (map mkTLA_Variable l)) 
  where mkTLA_Variable (TLA_Variable s) = [mk_AS_Ident s]
	mkTLA_Variable _ = []

mk_AS_RecordTypeElement :: SH_VarDecl -> AS_RecordElementType
mk_AS_RecordTypeElement (ty,i) =
    AS_RecordElementType epos (AS_Field i) (mk_AS_Type ty)

genTLA_TypeInv :: [TLA_GrpElement] -> AS_UnitDef
genTLA_TypeInv l = AS_OperatorDef upos 
		     (AS_OpHead (mk_AS_Ident "TypeInvariant") [])
		     (AS_LAND epos (concat $ map mkTLA_TypeInvEntry l))
  where mkTLA_TypeInvEntry (TLA_TypeInv (sh_type, i)) =
	    [AS_InfixOP epos AS_In (mk_AS_Ident i) (mk_AS_Type sh_type)]
	mkTLA_TypeInvEntry _ = []

mk_AS_Type (SH_Ty_UserDef _ ty) = mk_AS_Ident ty
mk_AS_Type (SH_Ty_UserDefOrNIL _ ty) = 
    AS_InfixOP epos AS_Cup (mk_AS_Type ty) 
		           (AS_DiscreteSet epos [mk_AS_Ident "NIL"])
mk_AS_Type (SH_Ty_Expr _ e) = e
mk_AS_Type (SH_Ty_SetOf _ ty) =
    AS_PrefixOP epos AS_SUBSET $ (mk_AS_Type ty)
mk_AS_Type (SH_Ty_SeqOf _ ty) =
    AS_OpApp epos (mk_AS_Ident "Seq") [mk_AS_Type ty]
mk_AS_Type (SH_Ty_PairOf _ tyA tyB) =
    AS_InfixOP epos AS_Times (mk_AS_Type tyA) (mk_AS_Type tyB)
mk_AS_Type (SH_Ty_Map _ tyA tyB) =
    AS_FunctionType epos (mk_AS_Type tyA) (mk_AS_Type tyB)
mk_AS_Type (SH_Ty_Enum _ [e]) = AS_DiscreteSet epos [mk_AS_Ident $ show e]
mk_AS_Type (SH_Ty_Enum _ l) = 
    combineInfix AS_Cup $ -- elements are quoted
		 map (\s -> AS_DiscreteSet epos [mk_AS_Ident $ show s]) l   
mk_AS_Type (SH_Ty_Union _ l) = 
    combineInfix AS_Cup $ map mk_AS_Type l 

genTLA_Init :: [TLA_GrpElement] -> AS_UnitDef
genTLA_Init l = AS_OperatorDef upos 
		     (AS_OpHead (mk_AS_Ident "Init") [])
		     (AS_LAND epos (concat $ map mkTLA_InitEntry l))
  where mkTLA_InitEntry (TLA_Init i e) =
	    [AS_InfixOP epos AS_EQ (mk_AS_Ident i) (mk_AS_Expr e)]
	mkTLA_InitEntry _ = []

genTLA_Next :: [TLA_GrpElement] -> AS_UnitDef
genTLA_Next l = AS_OperatorDef upos 
		     (AS_OpHead (mk_AS_Ident "Next") [])
		     (AS_LOR epos (concat $ map mkTLA_ActionEntry l))

genTLA_Fairness :: [TLA_GrpElement] -> AS_UnitDef
genTLA_Fairness l = 
  let vars = AS_Tuple epos (map mk_AS_Ident (concat $ map mkTLA_SpecEntry l))
      actions = concat $ map mkTLA_ActionEntry l
   in AS_OperatorDef upos 
	  (AS_OpHead (mk_AS_Ident "Fairness") [])
	  (AS_LAND epos (map (mkWF vars) actions))
  where mkWF vars action = AS_Fair False {-WF-} action vars

mkTLA_ActionEntry (TLA_Next singleMsgHndlr vartypes h) =
  let smhargs = if singleMsgHndlr 
		then [("_dummyNatT", "0")] 
		else []
   in if vartypes == []
      then [AS_OpApp epos (mk_AS_Ident h) [] ]
      else [AS_Quantified epos AS_Exist 
		  (map (\(t,i) -> 
	             AS_QBoundN [mk_AS_Ident i] (mk_AS_Ident t)) 
	             vartypes)
		  (AS_OpApp epos 
		   (mk_AS_Ident h) 
		   (map (\(_,i) -> mk_AS_Ident i) (vartypes ++ smhargs)))]
mkTLA_ActionEntry _ = []


genTLA_Spec :: [TLA_GrpElement] -> AS_UnitDef
genTLA_Spec l = let vars = AS_Tuple epos
			   (map mk_AS_Ident (concat $ map mkTLA_SpecEntry l))
		    fair = [mk_AS_Ident "Fairness"]
                 in AS_OperatorDef upos 
		     (AS_OpHead (mk_AS_Ident "Spec") [])
		       (AS_LAND epos 
			 ([ mk_AS_Ident "Init"
			  , (AS_PrefixOP epos AS_ALWAYS 
			      (AS_Stutter
			        (mk_AS_Ident "Next") vars))
			  ] ++ fair))

mkTLA_SpecEntry (TLA_Spec s) = [mkVar s]
mkTLA_SpecEntry _ = []
			    
genTLA_Wrapper :: [TLA_GrpElement] -> [AS_UnitDef]
genTLA_Wrapper l = concat $ map genTLA_Wrapper1 l
  where	genTLA_Wrapper1 (TLA_AS_Wrapper u) = [u]
	genTLA_Wrapper1 other = []

-- FIXME kramer@acm.org reto -- seems very broken, does this ever get called?
mk_AS_Expr (SH_ExprWrapper _ e) = e
mk_AS_Expr (SH_VIEW_REF _ s) = mk_AS_Ident $ "vIEW_"++s

combineInfix :: AS_InfixOp -> [AS_Expression] -> AS_Expression
combineInfix op [e] = e
combineInfix op (h:rest) = AS_InfixOP epos op h $ combineInfix op rest
combineInfix op [] = mk_AS_Ident "Uh"

genTLA_Boilerplate :: [(String, String)] -> [String] -> [AS_UnitDef]
genTLA_Boilerplate rolePairs roles =
    [genTLA_Boilerplate0 a b ((roles \\ [a]) \\ [b]) | 
         (a, b) <- rolePairs, a /= b] ++
    [genTLA_Boilerplate1 r (roles \\ [r]) | 
         (r,s) <- rolePairs, r == s] ++
    -- FIXME kramer@acm.org reto -- include this part only if needed
    genTLA_BoilerplateUnconditional 

-- multi destination message (!!) where sender and receiver role are DIFFERENT
genTLA_Boilerplate0 :: String -> String -> [String] -> AS_UnitDef
genTLA_Boilerplate0 senderRole receiverRole otherRoles =
  let sender = lower senderRole
      receiver = lower receiverRole
   in inlineOperatorDef $ subst 
      [("%s%", sender),
       ("%r%", receiver),
       ("%r_role%", receiverRole),
       ("%st_S%", mkVar senderRole),
       ("%st_R%", mkVar receiverRole)] $ unlines (
 [((mkDeliverMsgAction senderRole receiverRole) ++ "(%s%) =="),
  "  /\\ Len(%st_S%[%s%].g_obuf) > 0",
  "  /\\ LET e == Head(%st_S%[%s%].g_obuf)", 
  "          m == e[1]", -- message
  "          d == e[2]", -- dest set 
  "       IN IF d = {}",
  "          THEN /\\ %st_S%' = [%st_S% EXCEPT ![%s%].g_obuf = Tail(@)]"] ++ 
 ["               /\\ UNCHANGED <<" ++ (mkVar r) ++ ">>" | r <- otherRoles ++ [receiverRole]] ++
 ["          ELSE \\E p \\in d:",
  "            /\\ (p \\in %r_role%)",
  "            /\\ IF Cardinality(d) = 1",
  "                 THEN /\\ %st_S%' = [%st_S% EXCEPT ![%s%].g_obuf = Tail(@)]",
  "                      /\\ %st_R%' = [%st_R% EXCEPT ![p].g_inbox = ",
  "                                                        Append(@, m)]"] ++ 
 ["                      /\\ UNCHANGED <<" ++ (mkVar r) ++ ">>" | r <- otherRoles] ++
 ["                 ELSE /\\ %st_S%' = [%st_S% EXCEPT ![%s%].g_obuf =", 
  "                                        [@ EXCEPT ![1] = <<m, d \\ {p}>>]]",
  "                      /\\ %st_R%' = [%st_R% EXCEPT ![p].g_inbox =",
  "                                                        Append(@, m)]"] ++ 
 ["  /\\ UNCHANGED <<" ++ (mkVar r) ++ ">>" | r <- otherRoles])

-- multi destination message (!!) between instances of the SAME role
genTLA_Boilerplate1 :: String -> [String] -> AS_UnitDef
genTLA_Boilerplate1 role otherRoles =
    let r = lower role
     in inlineOperatorDef $ subst 
	[("%r%", r),
	 ("%R%", role),
	 ("%v%", mkVar role)] $ unlines (
 [((mkDeliverMsgAction role role) ++ "(%r%) =="),
  "  /\\ Len(%v%[%r%].g_obuf) > 0",
  "  /\\ LET e == Head(%v%[%r%].g_obuf)", 
  "          m == e[1]", -- message
  "          d == e[2]", -- dest set 
  "       IN IF d = {}",
  "          THEN /\\ %v%'=[%v% EXCEPT ![%r%].g_obuf = Tail(@)]"] ++
 ["               /\\ UNCHANGED <<" ++ (mkVar r) ++ ">>" | r <- otherRoles]++
 ["          ELSE \\E p \\in d:",
  "              /\\ (p \\in %R%)",
  "              /\\ IF Cardinality(d) = 1",
  "                    THEN /\\ %v%'=[%v% EXCEPT", 
  "                                       ![%r%].g_obuf = Tail(@),",
  "                                       ![p].g_inbox = Append(@, m)]"] ++
 ["                         /\\ UNCHANGED <<" ++ (mkVar r) ++ ">>" | r <- otherRoles]++
 ["                    ELSE /\\ %v%'=[%v% EXCEPT",
  "                                       ![%r%].g_obuf= [@ EXCEPT",
  "                                           ![1]= <<m,d \\ {p}>>],",
  "                                       ![p].g_inbox = Append(@, m)]"] ++
 ["                         /\\ UNCHANGED <<" ++ (mkVar r) ++ ">>" | r <- otherRoles])

-- has to be generated after definition of Msg
genTLA_BoilerplateUnconditional = 
     [inlineOperatorDef $ unlines ( 
   ["Majority(q,s) ==",
    "     Cardinality(q) > (Cardinality(s) \\div 2)"])] -- 3 \div 2 = 1
  -- used for selective receive
  ++ [inlineOperatorDef $ unlines ( 
   ["MsgPos(seq, t, Precond(_,_), agent) ==",
    "  IF \\E i \\in 1 .. Len(seq): ",        -- there's at least 1
    "            /\\ seq[i].type = t",           
    "            /\\ Precond(agent, seq[i])",
    "            /\\ \\A j \\in (1 .. i-1):", -- get smallest
    "                  ~(/\\ seq[j].type = t", 
    "                    /\\ Precond(agent, seq[j]))",
    "    THEN CHOOSE i \\in 1 .. Len(seq):",
    "            /\\ seq[i].type = t",
    "            /\\ Precond(agent, seq[i])",
    "            /\\ \\A j \\in (1 .. i-1):", -- get smallest
    "                  ~(/\\ seq[j].type = t", 
    "                    /\\ Precond(agent, seq[j]))",
    "    ELSE 0"])]
  ++ [inlineOperatorDef $ unlines ( 
   ["DropPos(seq, p) ==",
    "  SubSeq(seq, 1, p-1) \\o SubSeq(seq, p+1, Len(seq))"])]
--    "  [i \\in ( 1 .. Len(seq) ) \\ {p} |-> seq[i]]"])]

type Pattern = String
subst :: [(Pattern, String)] -> String -> String
subst pairlist s = 
    foldl (\s (pattern, subst) -> 
	subRegex (mkRegex pattern) s subst ) 
	s pairlist

mkDeliverMsgAction sender receiver = sender ++ "MultiSendTo" ++ receiver

substSH_Instr :: [(String, SH_ExprWrapper)] -> SH_Instr -> SH_Instr
substSH_Instr l = everywhere (mkT f)
  where f i@(AS_Ident _ _ s) =
	    case lookup s l of Nothing -> i
			       Just w  -> case w of SH_ExprWrapper _ e -> e
        f x = x

substAS_Expr :: [(String, String)] -> AS_Expression -> AS_Expression
substAS_Expr l = everywhere (mkT f)
  where f i@(AS_Ident _ _ s) =
	    case lookup s l of Nothing -> i
			       Just w  -> mk_AS_Ident w
        f x = x

---- HELPER -------------------------------------------------------------------
annSelectiveReceive :: [HandlerAnnotation] -> Bool
annSelectiveReceive = any annSelectiveReceive0
annSelectiveReceive0 (HandlerAnnotation "selective_receive" []) = True
annSelectiveReceive0 _ = False

mkVar role = "st_"++role
mkView role = "g_view_"++role

-- FIXME kramer@acm.org reto -- make action names unique by appending a 
-- SHA1 hash that is based on the label _and_ the guard expression.
-- Better to use the label and guard as a composit key into an ID table, and
-- number action names sequentially (extra points for not appending a number
-- if the name and guard combination is unique).
mkActionName :: String -> Maybe SH_ExprWrapper -> [SH_GuardedInstrList] 
	     -> String
mkActionName n guard l =   -- add instruction list into the hash mix to enable
    let g = case guard of  -- non-deterministic behaviour (actions same name)
	      (Just (SH_ExprWrapper p e)) -> show p ++ show e
	      Nothing -> ""
        h = hashStr $ capFirst n ++ g ++ show l
	h' = h -- take 12 (reverse h) -- try to get away with a few bits only
     in capFirst n ++ "_" ++ h'

-- concat ASCII values as numbers to avoid characters (e.g. / ! =) etc
-- that would not be valid TLA+ identifier elements.
hashStr :: String -> String
hashStr s = show (SHA1.hash $ fromJust $ Base64.decode s)

mk_AS_Separator = AS_Separator upos

mk_AS_Ident s = AS_Ident epos [] s

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos name line col = newPos name line col

upos = mkPos "foo" 0 0 
epos = (upos, Nothing, Nothing)

---- CTX QUERIES --------------------------------------------------------------

-- return all roles (globally)
allRoleNames :: Ctx -> [String]
allRoleNames (_, roleDecls) = listRoleNames roleDecls 

listRoleNames :: [Role] -> [String]
listRoleNames l = map (\(SH_RoleDef _ role _ _) -> role) l

globalRole = "GLOBAL"