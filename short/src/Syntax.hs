module Syntax where

import Debug.Trace as Trace
import Data.Map as Map hiding (map)
import Data.Set as Set hiding (map)
import Data.Generics

import Text.ParserCombinators.Parsec.Pos as PPos

import Language.TLAPlus.Syntax as TLASyntax

type SH_Info = PPos.SourcePos

data SH_Spec = SH_Spec { protocol    :: String,
			 concernList :: [SH_Concern] }
	       deriving (Eq, Ord, Show, Data, Typeable)

data SH_ExprWrapper = SH_ExprWrapper SH_Info 
		        TLASyntax.AS_Expression
		    | SH_VIEW_REF SH_Info 
		        String {-role-}
		      deriving (Eq, Ord, Show, Data, Typeable)

data SH_Type = SH_Ty_UserDef SH_Info String
	     | SH_Ty_UserDefOrNIL SH_Info SH_Type
	     | SH_Ty_Expr SH_Info AS_Expression -- code gen, unknown to parser
	     | SH_Ty_SetOf SH_Info SH_Type
	     | SH_Ty_SeqOf SH_Info SH_Type
	     | SH_Ty_PairOf SH_Info SH_Type SH_Type
	     | SH_Ty_Map SH_Info SH_Type SH_Type
	     | SH_Ty_Enum SH_Info [String]
	     -- FIXME kramer@acm.org reto -- add to TLACodeGen and Rewrite
					  -- and RewriteCont
	     | SH_Ty_Union SH_Info [SH_Type] -- \cup, unknown to parser so far
	       deriving (Eq, Ord, Show, Data, Typeable)

type SH_VarDecl = (SH_Type, String)

data SH_HookCaller = SH_HookCaller SH_Info 
		       String [SH_ExprWrapper]
		       deriving (Eq, Ord, Show, Data, Typeable)
data SH_HookCallee = SH_HookCallee SH_Info 
		       String [String]
		       deriving (Eq, Ord, Show, Data, Typeable)

data SH_Instr 
  = SH_I_MsgSend1 SH_Info
      String {- role name backpointer -}
      Bool {- multi send !! -} 
      Bool {- last gasp message, set rather than append obuf, 4codegen only -} 
      SH_ExprWrapper {-dest ident-} String {-msg type-} 
      [(String, SH_ExprWrapper)]
  | SH_I_ChangeState SH_Info [SH_ExprWrapper] {- assignments -}
  | SH_I_ChangeView SH_Info String SH_ExprWrapper
  | SH_I_Timercancel SH_Info String
  | SH_I_Timerrestart SH_Info SH_ExprWrapper {-duration-} String
  | SH_I_Shutdown SH_Info
  | SH_I_Drop SH_Info String {-message-}
  | SH_I_Reply SH_Info String {-msg type-} [(String, SH_ExprWrapper)]
  | SH_I_Let SH_Info [(String, SH_ExprWrapper)]
  | SH_I_Assert SH_Info SH_ExprWrapper String (Maybe [SH_ExprWrapper])
  | SH_I_Await SH_Info
      SH_RoleElement {- only handlers, they won't have guards and no gil -}
    -- only intended for code gen use
  | SH_I_ForeignChangeState SH_Info 
      String {- role -} 
      (Maybe AS_Expression) {- index var name into role array -}
      [SH_ExprWrapper] {- assignment -}
    -- only for code gen, used for CASE ... [] OTHER -> **FALSE** in 
    -- "short" tick action  
  | SH_I_FailTLAClause SH_Info
    -- only intended for code gen use, used to TLACodeGen to handle
    -- multiple !, or !! operations.
    -- FIXME kramer@acm.org reto -- really, this is needed only becuase there's
    -- no provision in TLACodeGen to merge SH_Instr (it's all geared towards
    -- splitting an instr into multiple TLA_I_Change operations.
  | SH_I_SendGroup SH_Info 
      [SH_Instr] -- list of SH_I_MsgSend1
  | SH_I_Break SH_Info -- marker for ANY handler merge
  | SH_I_Continue SH_Info -- marker for ANY handler merge
  | SH_I_Rewind SH_Info
      String {- role -} 
      String {- name of handler that coined the "PC" name -}
      (Maybe String) {- optional location -}
  | SH_I_State SH_Info {- note: inline state cannot be persistent -}
      SH_RoleElement {- only SH_State -}
  | SH_I_DoMeanwhile SH_Info
      [SH_Instr]
      [SH_RoleElement] -- handlers
    deriving (Eq, Ord, Show, Data, Typeable)

data SH_GuardedInstrList = SH_GuardedInstrList SH_Info
			     (Maybe SH_ExprWrapper) {- guard -}
			     (Maybe [SH_HookCaller]) {-label, args-}
			     [SH_Instr]
			   deriving (Eq, Ord, Show, Data, Typeable)
-- the tripple role/role/type must be unique in the merged specification.
-- FIXME - do enforce, the rewrite and Tag mechanism relies on this fact in
-- that we assume that a msg type will have only one receiving role type
-- (largely because I don't do type analysis on the dest expression of a send,
-- and hence have to assume that the message type conveys this information).
data SH_MsgDecl = SH_MsgDecl SH_Info 
		    SH_Type {- role type -}
		    SH_Type {- role type -}
		    String {-msg type-} 
		    [SH_VarDecl]
		    deriving (Eq, Ord, Show, Data, Typeable)

data SH_Concern = SH_Concern SH_Info String [SH_ConcernElement]
		  deriving (Eq, Ord, Show, Data, Typeable)
data SH_ConcernElement 
  = SH_Constant SH_Info [String]
  | SH_RoleList SH_Info [String]
  | SH_Interaction SH_Info 
      String {-name-}
      Bool {- enabled? -}
      [SH_Type] {-list of Roles, including SET<A> interact.-}
      [SH_InteractionElement]
      deriving (Eq, Ord, Show, Data, Typeable)

data SH_SL_MsgAnnKey 
  = SH_SL_MsgAnnColor | SH_SL_MsgAnnStyle
      deriving (Eq, Ord, Show, Data, Typeable)

data SH_SL_Ann 
  = SH_SL_MsgAnn
      String {- mtype -}
      [(SH_SL_MsgAnnKey,SH_ExprWrapper)]
      deriving (Eq, Ord, Show, Data, Typeable)

-- NOTE kramer@acm.org reto 
-- adding a new ie REQUIRES updates to Flatten and Merge !!!
data SH_InteractionElement
  = SH_IntraInteractionMsg SH_Info SH_MsgDecl
  | SH_Extend_Msg SH_Info 
      SH_Type {- role type -}
      SH_Type {- role type -}
      String {-msg type-} 
      [SH_VarDecl] {- added elements -} 
  | SH_RoleDef SH_Info
      String {-name-}
      [SH_VarDecl]
      [SH_RoleElement]
  | SH_VerbTLAOp SH_Info
      String {- name of interaction we're in -} 
      (Maybe [String]) {- override, names of interactions -}
      AS_UnitDef {- operator -}
  | SH_DisplaySwimlane SH_Info
      [SH_SL_Ann]
    deriving (Eq, Ord, Show, Data, Typeable)

data SH_FromKind
  = SH_FromAll String {- role type -}
  | SH_FromMaj String {- role type -}
  | SH_FromExp String {- role type -} SH_ExprWrapper
    deriving (Eq, Ord, Show, Data, Typeable)

data SH_WhereQuantifierKind = SH_All | SH_Some | SH_None
    deriving (Eq, Ord, Show, Data, Typeable)

data HandlerAnnotation = HandlerAnnotation String [String]
    deriving (Eq, Ord, Show, Data, Typeable)

data SH_RoleElement 
  = SH_Require SH_Info
      SH_ExprWrapper
  |  SH_State SH_Info
      Bool {- persistent? -}
      SH_VarDecl
      (Maybe SH_ExprWrapper) {-init-}
  | SH_ViewState SH_Info 
      SH_Type
      SH_ExprWrapper {-init-}
  | SH_Timer SH_Info
      String {- uppercase role name, used to find timers, RewriteTimer -}
      String {-id-}
  | SH_MsgHandler SH_Info
      [HandlerAnnotation]
      String {- lowercase role name, used to refer to variable role is bound -}
      (Maybe SH_ExprWrapper) {- WHEN predicate -}
      String {-msg type-}
      (Maybe [SH_HookCaller]) {-label, arg values-}
      Bool {-any message, i.e. do not match type as msg type-}
      (Maybe ( SH_FromKind
               {- WHERE expression and possible quantifier prefix -}
	     , Maybe (SH_ExprWrapper, SH_WhereQuantifierKind)))
      [SH_GuardedInstrList]
  | SH_CallHandler SH_Info
      String {- lowercase role name, used to refer to variable role is bound -}
      (Maybe SH_ExprWrapper) {- WHEN predicate -}
      String {-call label-}
      [(String, String)] {- arglist -}
      (Maybe [SH_HookCaller]) {-label, arg values-}
      [SH_GuardedInstrList]
  | SH_TimeoutHandler SH_Info
      String {- lowercase role name, used to refer to variable role is bound -}
      (Maybe SH_ExprWrapper) {- WHEN predicate -}
      String {-id-}
      (Maybe [SH_HookCaller]) {-label, arg values-}
      [SH_GuardedInstrList]
  | SH_CrashHandler SH_Info 
      [HandlerAnnotation]
      String {- lowercase role name, used to refer to variable role is bound -}
      (Maybe SH_ExprWrapper) {- WHEN predicate -}
      String {-role name-}
      String {-id-}
      (Maybe [SH_HookCaller]) {-label, arg values-}
      [SH_GuardedInstrList]
  | SH_Every SH_Info {- Note: has no guard -}
      String {- lowercase role name, used to refer to variable role is bound -}
      (Maybe SH_ExprWrapper) {- WHEN predicate -}
      SH_ExprWrapper {- timer period -}
      (Maybe [SH_HookCaller]) {-label, arg values-}
      [SH_GuardedInstrList]
  | SH_Extend_Hook SH_Info 
      String {- lowercase role name, used to refer to variable role is bound -}
      [SH_HookCallee] {- label, and argument name list -}
      [SH_GuardedInstrList]
  | SH_Tag SH_Info
      String   {- role name backpointer -}
      [String] {-msg type-}
      Bool     {- FIXME: any msg type, mk dest role Nothing if this is false -}
      SH_Type  {- TO destination role -}
      [(SH_VarDecl, SH_ExprWrapper)]
  | SH_Once SH_Info
      String   {- role name backpointer -}
      (Maybe SH_ExprWrapper) {- WHEN predicate -}
      String {-label-}
      (Maybe [SH_HookCaller]) {-label, arg values-}
      [SH_GuardedInstrList]
  | SH_UseState SH_Info
      String String
  | SH_UseMsg SH_Info
      String String
  | SH_WhenBlock SH_Info
      SH_ExprWrapper {- predicate -}      
      [SH_RoleElement]      
    deriving (Eq, Ord, Show, Data, Typeable)

---- Queries ------------------------------------------------------------------
isTypeSet :: SH_Type -> Bool
isTypeSet (SH_Ty_SetOf _ _) = True
isTypeSet _ = False

isMultiDestMsg :: SH_MsgDecl -> Bool
isMultiDestMsg (SH_MsgDecl _ _ dest _ _) = isTypeSet dest
