module SyntaxPretty 
  ( prettyPrintSH,
    ppInteractionElement, ppMsgDecl, ppConcernElement, ppTy)
where

import Data.List (find, elemIndex)
import Data.Map as Map (foldWithKey)
import Data.Set as Set (elems, fromList)

import Prelude hiding ((<$>))

import Text.PrettyPrint.Leijen

import Syntax

import Language.TLAPlus.Pretty as TLASyntaxPretty (ppE, ppUnit)

prettyPrintSH :: SH_Spec -> String
prettyPrintSH spec =
    showWidth 79 $
        text "protocol" <+> text (protocol spec) 
    <+> ppConcernList (concernList spec)
    where showWidth :: Int -> Doc -> String 
	  showWidth w doc = displayS (renderPretty 0.9 w doc) ""

ppConcernList :: [SH_Concern] -> Doc
ppConcernList l = vcat $ map ppConcern l

ppConcern :: SH_Concern -> Doc
ppConcern (SH_Concern _p name l) =
  text "concern" <+> text name <+> text "{" 
  <$> indent ilevel (align $ ppConcernElementList l)
  <$> text "}"

ppConcernElementList :: [SH_ConcernElement] -> Doc
ppConcernElementList l = vcat $ map ppConcernElement l

ppConcernElement :: SH_ConcernElement -> Doc
ppConcernElement (SH_Constant _p l) =
    text "constant" </> align(fillCat $ punctuate comma (map text l))
ppConcernElement (SH_RoleList _p l) =
    text "roles" </> align(fillCat $ punctuate comma (map text l))
ppConcernElement (SH_Interaction _p name enabled roles l) = 
    (if enabled then empty else text "disabled") <//>
    text "interaction" <+> text name
    <//> (if roles == [] 
	  then empty
	  else parens (cat $ punctuate comma (map ppTy roles))) 
    <+> text "{"
    <$> indent ilevel (align (ppInteractionElementList l))
    <$> text "}"  

ppInteractionElementList :: [SH_InteractionElement] -> Doc
ppInteractionElementList l = vcat $ map ppInteractionElement l

ppInteractionElement :: SH_InteractionElement -> Doc
ppInteractionElement (SH_IntraInteractionMsg _p m) = 
    ppMsgDecl m
ppInteractionElement (SH_Extend_Msg p from to mtype l) = 
    text "extend" <+> ppMsgDecl (SH_MsgDecl p from to mtype l)
ppInteractionElement (SH_RoleDef _p name paramlist l) =
    text "role" <+> text name 
    <//> (if paramlist == [] 
	  then empty
	  else parens (align (cat $ punctuate comma 
			              (map (\(t,i) -> ppTy t <+> text i) 
 		                           paramlist))))
        <+> text "{"
        <$> indent ilevel (align (ppStatementList l))
        <$> text "}"
ppInteractionElement (SH_VerbTLAOp _p i o u) =
    (case o of 
       Nothing -> empty 
       Just lo -> text "override " <+> 
		  vcat (punctuate comma (map (\o -> text o) lo))) <+>
    text "tla {" <+> indent ilevel (align $ ppUnit u) <+> text "}" 

ppInteractionElement (SH_DisplaySwimlane _ anns) =
        text "display swimlane" <+> text "{" 
    <$> indent ilevel (align (vcat $ map ppDisplaySwimlaneAnn anns))
    <$> text "}"

ppDisplaySwimlaneAnn :: SH_SL_Ann -> Doc
ppDisplaySwimlaneAnn (SH_SL_MsgAnn mtype l) =
        text "msg" <+> text mtype
    <+> align (cat $ punctuate comma (
	         map (\(k,e) -> 
                        text (ppSH_SL_MsgAnnKey k) <+> text "=" <+> ppExpr e)
                     l))

ppSH_SL_MsgAnnKey SH_SL_MsgAnnColor = "color"
ppSH_SL_MsgAnnKey SH_SL_MsgAnnStyle = "style"

ppMsgDecl :: SH_MsgDecl -> Doc
ppMsgDecl (SH_MsgDecl _p r1 r2 t l) =
    text "msg" 
    <+> ppTy r1 <+> text "->"
    <+> ppTy r2 <+> text t 
    <//> parens (align $ cat (punctuate comma 
		                (map (\(tt,i) -> ppTy tt <+> text i) l)))

ppStatementList :: [SH_RoleElement] -> Doc
ppStatementList l = vcat $ map (ppStatement True) l

ppStatement :: Bool -> SH_RoleElement -> Doc
ppStatement _ (SH_Require _p e) =
    text "require" <+> ppExpr e
ppStatement _ (SH_State _p persistent (t,id) e) =
    (if persistent then text "persistent " else empty) <//>
    text "state" <+> ppTy t <+> 
	 (case e of 
	   Nothing  -> text id
	   (Just e) -> text id <+> text "=" <+> ppExpr e)
ppStatement _ (SH_ViewState _p t init) =
        text "state" <+> text "views" <+> ppTy t 
    <+> text "=" <+> ppExpr init
ppStatement _ (SH_Timer _p _ t) =
    text "state" <+> text "timer" <+> text t

ppStatement h (SH_CallHandler _p _role when label msgrec hook l) =
            (case when of 
	      Nothing  -> handleText h
	      (Just e) -> text "when" <+> ppExpr e <+> text "handle") 
        <+> text "event"
	<+> (case msgrec of 
	      [] -> text label <//> text "()"
	      _  -> text label <//> ppStringPairList msgrec)
        <+> (case hook of 
	       Nothing -> empty 
	       (Just hcaller) -> ppHookCallerList hcaller)
        <+> handleGIL h (ppGIL l)
ppStatement h (SH_TimeoutHandler _p _role when t hook l) =
            (case when of
	      Nothing  -> handleText h
	      (Just e) -> text "when" <+> ppExpr e <+> text "handles") 
        <+> text "timeout"
        <+> text t
        <+> (case hook of 
	       Nothing -> empty 
	       (Just hcaller) -> ppHookCallerList hcaller)
	<+> handleGIL h (ppGIL l)
ppStatement h (SH_CrashHandler _p ann _role when role id hook l) =
            (if ann == [] 
	     then empty
	     else text "using [" <//> ppHandlerAnnList ann <//> text "]" )
        <+> (case when of
	      Nothing  -> handleText h
	      (Just e) -> text "when" <+> ppExpr e <+> text "handles") 
        <+> text "crash"
        <+> text role
        <+> text id
        <+> (case hook of 
	       Nothing -> empty 
	       (Just hcaller) -> ppHookCallerList hcaller)
	<+> handleGIL h (ppGIL l)
ppStatement h (SH_MsgHandler _p ann _role when t hook any from_scope l) =
            (if ann == [] 
	     then empty
	     else text "using [" <//> ppHandlerAnnList ann <//> text "]" )
        <+> (case when of 
	      Nothing  -> handleText h
	      (Just e) -> text "when" <+> ppExpr e <+> text "handles") 
	<+> (if any then text "any" <+> text "msg" else text "msg") <+> text t
	<+> (case from_scope of
	       Nothing -> empty
	       Just (SH_FromMaj v, w) -> 
	         (text "from" <+> text "majority" 
		  <//> parens (text v) 
		  <+> (case w of Nothing -> empty
		                 Just (e, q) -> 
		                   text "where" <+> ppWhereExpr q e))
	       Just (SH_FromExp t e, w) -> 
	         (text "from" <+> text "expr"
		  <//> parens (text t <+> text "," <+> ppExpr e) 
		  <+> (case w of Nothing -> empty
		                 Just (e, q) -> 
		                   text "where" <+> ppWhereExpr q e))
	       Just (SH_FromAll v, w) -> 
	         (text "from" <+> text "all"
		  <//> parens (text v)
		  <+> (case w of Nothing -> empty
		                 Just (e,q) -> 
                                   text "where" <+> ppWhereExpr q e)))
        <+> (case hook of
	       Nothing -> empty 
	       (Just hcaller) -> ppHookCallerList hcaller)
	<+> handleGIL h (ppGIL l)
ppStatement _ (SH_Every _p _role when e hook l) =
            text "every" <+> ppExpr e 
        <+> (case when of 
	      Nothing  -> empty
	      (Just we) -> text "while" <+> ppExpr we) 
        <+> (case hook of 
	       Nothing -> empty 
	       (Just hcaller) -> ppHookCallerList hcaller)
        <+> ppGIL l
ppStatement _ (SH_Extend_Hook _p _role hookcalleeList l) =
            text "extend" <+> ppHookCalleeList hookcalleeList 
	<+> ppGIL l
ppStatement _ (SH_Tag _p _r t any destrole ass) =
	    text "tag"
	<+> (if any then text "any" <+> text "msg" else text "msg") 
        <+> vcat (punctuate comma $ map text t)
	<+> text "TO" <+> ppTy destrole 
        <+> text "{" </> ppTypedAssignList ass <+> text "}"
ppStatement _ (SH_Once _p _role when label hook l) =
            (case when of
	      Nothing  -> text "handles" <+> text "once"
	      (Just e) -> text "when" <+> ppExpr e <+> 
	                  text "handles" <+> text "once") 
        <+> text label
        <+> (case hook of 
	       Nothing -> empty 
	       (Just hcaller) -> ppHookCallerList hcaller)
	<+> ppGIL l
ppStatement _ (SH_UseState _p field interact) =
    text "use" <+> text "state" <+> text field <+> text "of" <+> text interact
ppStatement _ (SH_UseMsg _p t interact) =
    text "use" <+> text "msg" <+> text t <+> text "of" <+> text interact
ppStatement _ (SH_WhenBlock _p pred l) =
        text "when" <+> ppExpr pred <+> text "{"
    <$> indent ilevel (align (ppStatementList l)) 
    <$> text "}"

handleText :: Bool -> Doc
handleText True  = text "handle"
handleText False = text "await"

handleGIL :: Bool -> Doc -> Doc
handleGIL True doc = doc
handleGIL False _  = empty

ppInstr :: SH_Instr -> Doc
ppInstr (SH_I_MsgSend1 _p _role m _last dest t assign) =
    ppExpr dest <+> (if m then text "!!" else text "!") <+> 
	   text t <//> parens (ppAssignList assign)
ppInstr (SH_I_ChangeView _p i e) =
    text "changeview" <+> text i <+> text "=" <+> ppExpr e
ppInstr (SH_I_ChangeState _p le) =
    text "change" <+> cat (punctuate comma (map ppExpr le))
ppInstr (SH_I_Timercancel _p t) =
    text "timercancel" <+> text t
ppInstr (SH_I_Timerrestart _p e t) =
    text "timerrestart" <+> text t <+> text "duration" <+> ppExpr e
ppInstr (SH_I_Shutdown _p) =
    text "shutdown"
ppInstr (SH_I_Drop _p m) =
    text "drop" <+> text m
ppInstr (SH_I_Reply _p t assign) =
    text "reply" <+> text t <//> parens (ppAssignList assign)
ppInstr (SH_I_Let _p bindings) =
    align(text "let" 
      <+> (vcat (punctuate comma 
		 (map ( \ (i,e) -> 
			text i <+> text "=" <+> ppExpr e) bindings))))
ppInstr (SH_I_Assert _p e s l) =
    text "assert" <+> parens (cat $ punctuate comma 
	                ([ppExpr e, text $ show s] ++ 
			 (case l of 
			  Nothing -> [] 
			  (Just l') -> map ppExpr l')))
-- only intended for code gen use
ppInstr (SH_I_ForeignChangeState _p role var le) =
        text "foreignchange" <+> cat (punctuate comma (map ppExpr le))
    <+> text "in" <+> text role
    <+> (case var of 
           Nothing -> empty 
	   (Just e) -> text "var" <+> ppE e)
ppInstr (SH_I_FailTLAClause _p) =
    text "failtlaclause"
ppInstr (SH_I_SendGroup _p l) =
    text "send-group" <+> (ppInstrList l)
ppInstr (SH_I_Break _p) =
    text "break"
ppInstr (SH_I_Continue _p) =
    text "continue"
ppInstr (SH_I_Rewind _p _role id Nothing) =
    text "rewind" <+> text id
ppInstr (SH_I_Rewind _p _role id (Just loc)) =
    text "rewind" <+> text id <+> text "to" <+> text loc
ppInstr (SH_I_Await _p handler) =
    ppStatement False handler
ppInstr (SH_I_State _p state) =
    ppStatement False {- don't care -} 
		state
ppInstr (SH_I_DoMeanwhile _ il handlers) =
        text "whilein" <+> text "{" 
    <$> indent ilevel (align (ppInstrList il)) 
    <$> text "} do {"
    <$> indent ilevel (align (ppStatementList handlers)) 
    <$> text "}"

ppWhereExpr SH_All  e = text "all"  <+> ppExpr e
ppWhereExpr SH_Some e = text "some" <+> ppExpr e
ppWhereExpr SH_None e = text "none" <+> ppExpr e

ppTy (SH_Ty_UserDef _p t) = text t
ppTy (SH_Ty_UserDefOrNIL _p t) = text "Nil<" <//> ppTy t <//> text ">"
ppTy (SH_Ty_Expr _p e) = ppE e
ppTy (SH_Ty_SetOf _p t) = cat [text "Set<", ppTy t, text ">"]
ppTy (SH_Ty_SeqOf _p t) = cat [text "Seq<", ppTy t, text ">"]
ppTy (SH_Ty_PairOf _p a b) = cat [text "Pair<", ppTy a, text ",", 
				                ppTy b, text ">"]
ppTy (SH_Ty_Map _p a b) = cat [text "Map<", ppTy a, text ",", ppTy b, text ">"]
ppTy (SH_Ty_Enum _p l) = text "Enum" <//> parens (cat $ punctuate (char '|') 
			                                  (map text l))
ppTy (SH_Ty_Union _p l) = cat $ punctuate (char '+') (map (parens . ppTy) l)

ppInstrList :: [SH_Instr] -> Doc
ppInstrList l = align (vcat $ map ppInstr l)

ppGIL :: [SH_GuardedInstrList] -> Doc
ppGIL l = let g = case l of -- special handling, if there's only one guard
		     [gil] -> ppGuardedInstrList gil
		     _ -> vcat $ map ppGuardedInstrList l
	    in text "{" <$> indent ilevel (align g) <$> text "}"

ppHookCallerList :: [SH_HookCaller] -> Doc
ppHookCallerList l = cat $ punctuate comma (map ppHookCaller l)

ppHookCaller :: SH_HookCaller -> Doc
ppHookCaller (SH_HookCaller _p label exprs) =
    cat [text "@", text label, 
	 if exprs == [] 
	 then empty 
	 else parens $ cat (punctuate comma (map ppExpr exprs))]

ppHookCalleeList :: [SH_HookCallee] -> Doc
ppHookCalleeList l = cat $ punctuate comma (map ppHookCallee l)

ppHookCallee :: SH_HookCallee -> Doc
ppHookCallee (SH_HookCallee _p label argnames) =
    cat [text "@", text label, 
	 if argnames == [] 
	 then empty 
	 else parens $ cat (punctuate comma (map text argnames))]

ppHandlerAnnList :: [HandlerAnnotation] -> Doc
ppHandlerAnnList l = cat $ punctuate comma (map ppHandlerAnn l)

ppHandlerAnn :: HandlerAnnotation -> Doc
ppHandlerAnn (HandlerAnnotation a l) =
    cat [text a, 
	 if l == [] 
	 then empty 
	 else parens $ cat (punctuate comma (map text l))]

ppGuardedInstrList :: SH_GuardedInstrList -> Doc
ppGuardedInstrList (SH_GuardedInstrList _p g label l) =
    (case g of 
       Nothing -> empty -- debugging: (text "<<< EMPTY >>>")
       (Just g) -> text "|" <+> ppExpr g <+> text "->")
    <+> (case label of 
	 Nothing -> empty 
	 (Just hcaller) -> ppHookCallerList hcaller)
    </> ppInstrList l

whenGuard :: Maybe SH_ExprWrapper -> Doc
whenGuard Nothing = text ""
whenGuard (Just e) = text "when" <+> ppExpr e

ppAssignList :: [(String, SH_ExprWrapper)] -> Doc
ppAssignList l = align( fillCat $ punctuate comma 
		                    (map ( \(s,e) -> 
				       text s <+> text "=" <+> ppExpr e)
		                       l))

ppTypedAssignList :: [(SH_VarDecl, SH_ExprWrapper)] -> Doc
ppTypedAssignList l = align( fillCat $ punctuate comma 
		               (map ( \((t,s),e) -> 
				             ppTy t <+> text s 
				         <+> text "=" <+> ppExpr e)
		                      l))

ppStringPairList :: [(String, String)] -> Doc
ppStringPairList l = align( parens (fillCat $ punctuate comma 
			      (map ( \(t,i) -> text t <+> text i) l) ))

ppExpr :: SH_ExprWrapper -> Doc
ppExpr (SH_ExprWrapper _p e) = 
    TLASyntaxPretty.ppE e
ppExpr (SH_VIEW_REF _p role) = 
    text "view" <//> parens (text role)

ilevel = 2
