module RewriteLifecycle where

import Data.List (nub, (\\), groupBy )
import Data.Char (toLower, toUpper)
import qualified Data.Set as Set
import Data.Generics
import Data.Typeable as Typeable
import Syntax
import Flatten
import TLACodeGen(mk_AS_Type, mkVar, mkView, typeKernel)
import Debug.Trace(trace)

import Text.Parsec.Pos as PPos

import Language.TLAPlus.Syntax as TLASyntax

import RewriteTimer(allRoles, hasGlobalRole)
import GenCFG(crashStartControlConst)

rewriteLifecycle :: SH_FL_Spec -> SH_FL_Spec
rewriteLifecycle spec =
      removeSHUTDOWN
    $ rewriteCrashHandlers
    $ rewriteSHUTDOWN -- relies on the presence of crash handler, do first!
    $ rewriteLifecycle0
    $ addCrashMsgDecl
    $ addCrashStartControlConst
    $ rewriteResetNonPersistentState
    $ mergeCrashHandlers
    $ addStutterIfNothingRunning spec

-- if all roles can crash, add an action that can stutter to avoid deadlock
-- reports
addStutterIfNothingRunning :: SH_FL_Spec -> SH_FL_Spec
addStutterIfNothingRunning spec =
           -- FIXME kramer@acm.org reto -- shouldn't I include the Shutdown?
  if [] /= spontanousCrashers spec
    then let g = SH_RoleDef upos "GLOBAL" [] []
             spec' = if hasGlobalRole spec
                     then spec
                     else spec { roleDecl = g : (roleDecl spec) }
          in everywhere (mkT f) spec'
    else spec
  where f (SH_RoleDef _ "GLOBAL" vars l) =
            let guard = AS_LAND epos $ map mkNotRunningClause
                                           (spontanousCrashers spec)
                stutter = SH_CallHandler upos "global"
                            (Just $ SH_ExprWrapper upos guard)
                            "StutterWhenNothingRunning" [] Nothing []
                -- FIXME kramer@acm.org reto -- the global state has to have
                -- some state since otherwise, there's UNCHANGED<st_GLOBAL>
                -- references in the "Stutter..." action, but no st_GLOBAL
                -- will have been generated.
                fakeState = SH_State upos False
                              (SH_Ty_UserDef upos "BOOLEAN",
                               "g_dummy")
                              (Just $ SH_ExprWrapper upos (AS_Bool epos False))
             in SH_RoleDef upos "GLOBAL" vars (l ++ [stutter] ++ [fakeState])
        f x = x
        mkNotRunningClause role =
            let bounds = [AS_QBoundN [mk_AS_Ident $ lower role]
                                     (mk_AS_Ident role)]
             in AS_Quantified epos AS_All bounds $
                  AS_PrefixOP epos AS_Not $
                    (AS_InfixOP epos AS_DOT
                       (AS_InfixOP epos AS_FunApp
                          (mk_AS_Ident $ mkVar role)
                          (AS_FunArgList epos
                             [mk_AS_Ident $ lower role]))
                       (mk_AS_Ident "g_running"))

-- FIXME kramer@acm.org reto -- cop out. Should really remove this instr!
removeSHUTDOWN :: SH_FL_Spec -> SH_FL_Spec
removeSHUTDOWN spec = everywhere (mkT f) spec
  where f (SH_GuardedInstrList info guard label l) =
            SH_GuardedInstrList info guard label (filter (not . isShutdown) l)
        isShutdown (SH_I_Shutdown upos) = True
        isShutdown _ = False

rewriteSHUTDOWN :: SH_FL_Spec -> SH_FL_Spec
rewriteSHUTDOWN spec = everywhere (mkT f) spec
    where f (SH_MsgHandler _ ann role when mtype label any from ginstr) =
              SH_MsgHandler upos ann role when mtype label any from
                (rewriteShutdown0 spec role ginstr)
          f (SH_CallHandler _ role when label args hook ginstr) =
              SH_CallHandler upos role when label args hook
                (rewriteShutdown0 spec role ginstr)
          f (SH_TimeoutHandler _ role when name hook ginstr) =
              SH_TimeoutHandler upos role when name hook
                (rewriteShutdown0 spec role ginstr)
          f (SH_CrashHandler _ ann role when remoteRole id hook ginstr) =
              SH_CrashHandler upos ann role when remoteRole id hook
                (rewriteShutdown0 spec role ginstr)
          f (SH_Every _ role guard period hook ginstr) =
             SH_Every upos role guard period hook
                (rewriteShutdown0 spec role ginstr)
          f (SH_Once _ role guard label hook ginstr) =
             SH_Once upos role guard label hook
                (rewriteShutdown0 spec role ginstr)
          f (SH_Extend_Hook _ role hook ginstr) =
              SH_Extend_Hook upos role hook
                (rewriteShutdown0 spec role ginstr)
          f x = x
          rewriteShutdown0 spec role l =
              if hasShutdown l
              then appendIL (genLastActions spec (upper role)) l
              else l
            where hasShutdown l = (everything (++) ([] `mkQ` h)) l /= []
                  h (SH_I_Shutdown _) = [True]
                  h _ = []

addCrashMsgDecl ::  SH_FL_Spec -> SH_FL_Spec
addCrashMsgDecl spec =
    let mlist = map (\r -> let hrl = crashHandlingRolesFor spec r
                            in map (\hr -> SH_MsgDecl upos
                                             (SH_Ty_UserDef upos r)
                                             (SH_Ty_SetOf upos $
                                                SH_Ty_UserDef upos $ upper hr)
                                             (mkCrashMsgName r $ upper hr) [])
                                   hrl)
                    (crashableRoles spec)
     in spec { msgDecl = (msgDecl spec) ++ (concat mlist) }

rewriteCrashHandlers :: SH_FL_Spec -> SH_FL_Spec
rewriteCrashHandlers spec = everywhere (mkT f) spec
  where f (SH_CrashHandler info ann role when n id hook l) =
            let m = mkCrashMsgName n (upper role)
                e = AS_InfixOP epos AS_DOT
                      (mk_AS_Ident m)
                      (mk_AS_Ident "sender")
             in SH_MsgHandler info ann role when m hook False Nothing
                  $ prependIL [SH_I_Let upos [(id, SH_ExprWrapper upos e)]] l
        f x = x

mergeCrashHandlers :: SH_FL_Spec -> SH_FL_Spec
mergeCrashHandlers spec = everywhere (mkT f) spec
  where f (SH_RoleDef _ name vars l) =
            let ch = crashHandlersInRole (lower name) spec
                gch = groupCrashHandlers ch
                mch = map fuseCrashHandlers gch
             in SH_RoleDef upos name vars ((l \\ ch) ++ mch)
        f x = x
        groupCrashHandlers :: [SH_RoleElement] -> [[SH_RoleElement]]
        groupCrashHandlers l = groupBy sameCrashHandler l
          where sameCrashHandler
                 (SH_CrashHandler _ _ _ when remoteRole _ _ _)
                 (SH_CrashHandler _ _ _ when' remoteRole' _ _ _) =
                     (wipePos when == wipePos when') &&
                     (remoteRole == remoteRole')
        fuseCrashHandlers :: [SH_RoleElement] -> SH_RoleElement
        fuseCrashHandlers l@((SH_CrashHandler _ ann role when
                                              remoteRole id hook ginstr):_) =
          -- NOTE kramer@acm.org reto -- subst of crash handler vars not
                                      -- needed since we use let x=cmsg.sender
          let ginstr' = concat $ concatMap getGIL l
           in SH_CrashHandler upos ann role when remoteRole id hook ginstr'
        getGIL (SH_CrashHandler _ _ _ _ _ _ _ l) = [l]
        getGIL _ = []
        wipePos :: (Maybe SH_ExprWrapper) -> (Maybe SH_ExprWrapper)
        wipePos l = everywhere (mkT f) l
            where f x | Typeable.typeOf x ==
                        Typeable.typeOf upos = -- i.e. is it a SourcePos?
                          upos -- make them all the same
                  f x | otherwise = x

rewriteLifecycle0 :: SH_FL_Spec -> SH_FL_Spec
rewriteLifecycle0 spec =
        -- the crash observing roles need a g_running attribute since the
        -- crashing role will only send notifications to those listeners that
        -- are running, and thus filters on their g_running attribute.
    let obs = concat $ map (crashHandlingRolesFor spec)
                           (crashableRoles spec ++ spontanousCrashers spec)
     in everywhere (mkT (f (nub $ crashableRoles spec ++ obs,
                            spontanousCrashers spec))) spec
  where f (croles, spontanousCrash) r@(SH_RoleDef _ name _ _) =
            -- FIXME kramer@acm.org reto --
            -- the "lower" use is a hack since obs = crashHandlingRolesFor ...
            -- returns the role name in small case.
            if elem (lower name) (map lower croles)
            then rewriteRole spontanousCrash spec r
            else r
        f _ x = x

rewriteRole :: [String] -> SH_FL_Spec -> SH_InteractionElement
            -> SH_InteractionElement
rewriteRole spontanousCrashers spec ie = everywhere (mkT (f spec)) ie
  where f spec (SH_RoleDef _ name vars l) =
            let st = SH_State upos False
                       (SH_Ty_UserDef upos "BOOLEAN", "g_running")
                       (Just $ SH_ExprWrapper upos
                          (AS_PrefixOP epos AS_Not
                             (AS_InfixOP epos AS_In
                                (mk_AS_Ident $ lower name)
                                (mk_AS_Ident ("InitDown" ++ name)))))
                lifec = SH_State upos False
                         (SH_Ty_UserDef upos "{0,1,2}", "g_lifecycle")
                         (Just $ SH_ExprWrapper upos (AS_Num epos 0))
                inc = SH_I_ChangeState upos
                        [SH_ExprWrapper upos
                         (AS_InfixOP epos AS_EQ
                          (mk_AS_Ident "g_lifecycle")
                          (AS_InfixOP epos AS_Plus
                             (AS_OldVal)
                             (AS_Num epos 1)))]
                crasher = if elem name spontanousCrashers
                          then [SH_CallHandler upos (lower name)
                                 (Just $ (SH_ExprWrapper upos
                                           (AS_LAND epos [
                                             (mk_AS_Ident "g_running"),
                                             (AS_InfixOP epos AS_LT
                                                (mk_AS_Ident "g_lifecycle")
                                                (AS_Num epos 2)),
                                             AS_InfixOP epos AS_In
                                                (mk_AS_Ident $ lower name)
                                                (mk_AS_Ident ("Crash" ++ name))])))
                                 ("do_crash_" ++ name) []
                                -- we offer a well know hook name to allow for
                                -- extensions
                                 (Just [SH_HookCaller upos
                                          ("hook_do_crash_" ++ name)
                                          [SH_ExprWrapper upos $
                                             mk_AS_Ident "self"]])
                                 [SH_GuardedInstrList upos Nothing Nothing
                                    (genLastActions spec name ++ [inc])]]
                          else []
                flipOn = SH_I_ChangeState upos
                         [SH_ExprWrapper upos
                           (AS_InfixOP epos AS_EQ
                             (mk_AS_Ident "g_running")
                             (AS_Bool epos True))]
                clearInbox = SH_I_ChangeState upos
                             [SH_ExprWrapper upos
                              (AS_InfixOP epos AS_EQ
                               (mk_AS_Ident "g_inbox")
                               (AS_Tuple epos []))]
                -- let each role instance start at most once to avoid flapping
                starter = if  elem name spontanousCrashers
                          then [SH_CallHandler upos (lower name)
                                 (Just $ (SH_ExprWrapper upos
                                           (AS_LAND epos [
                                             (AS_PrefixOP epos AS_Not
                                                  (mk_AS_Ident "g_running")),
                                             (AS_InfixOP epos AS_LT
                                                (mk_AS_Ident "g_lifecycle")
                                                (AS_Num epos 2))])))
                                 ("do_start_" ++ name) []
                                -- we offer a well know hook name to allow for
                                -- extensions
                                 (Just [SH_HookCaller upos
                                          ("hook_do_start_" ++ name)
                                          [SH_ExprWrapper upos $
                                             mk_AS_Ident "self"]])
                                 [SH_GuardedInstrList upos
                                    (Just $ SH_ExprWrapper upos
                                         (AS_InfixOP epos AS_In
                                           (mk_AS_Ident $ lower name)
                                           (mk_AS_Ident ("Start" ++ name))))
                                    Nothing
                                    [inc, flipOn, clearInbox],
                                  SH_GuardedInstrList upos
                                    (Just $ SH_ExprWrapper upos
                                       (mk_AS_Ident "otherwise"))
                                    Nothing
                                    [SH_I_FailTLAClause upos]]]
                          else []
             in SH_RoleDef upos name vars $
                  (predicateWhen (mk_AS_Ident "g_running") l) ++
                    [st, lifec] ++ crasher ++ starter
        f _ x = x

addCrashStartControlConst :: SH_FL_Spec -> SH_FL_Spec
addCrashStartControlConst spec =
    let roles = allRoles spec \\ ["GLOBAL"]
        cs = concat $ map (\r -> mkC r crashStartControlConst) roles
     in spec { constant = constant spec ++ cs }
  where mkC r prefixes = map (\name -> name ++ r) prefixes

rewriteResetNonPersistentState :: SH_FL_Spec -> SH_FL_Spec
rewriteResetNonPersistentState spec =
    let roles = nub $ crashableRoles spec ++ spontanousCrashers spec
     in everywhere (mkT (f roles)) spec
  where f roles r@(SH_RoleDef _ name _ _) =
            if elem (lower name) (map lower roles)
            then addResetNonPersistentState spec r
            else r
        f _ x = x

addResetNonPersistentState :: SH_FL_Spec -> SH_InteractionElement
                           -> SH_InteractionElement
addResetNonPersistentState spec ie = everywhere (mkT (f spec)) ie
    where f spec (SH_RoleDef _ name vars l) =
              let resets = everything (++) ([] `mkQ` h) l
                  extension = SH_Extend_Hook upos
                                (lower name)
                                [SH_HookCallee upos
                                   ("hook_do_crash_"++name) ["self"]]
                                [SH_GuardedInstrList upos Nothing Nothing
                                   resets]
                  l' = if resets == [] then [] else [extension]
               in SH_RoleDef upos name vars $ l ++ l'
          h (SH_State _p persistent (_ty, varname) init)
              | (not persistent) &&
                (varname /= "g_running") =
                  -- we know rewriteStateInit has filled in the
                  -- init expression (see short).
                  let (Just (SH_ExprWrapper _ i)) = init
                      assign = SH_ExprWrapper upos $ AS_InfixOP epos AS_EQ
                                                       (mk_AS_Ident varname)
                                                       i
                   in [SH_I_ChangeState upos [assign]]
              | otherwise  = []
          h (SH_ViewState _p ty init)
              | True = -- always non-persisent, might change in future
                  let (SH_ExprWrapper _ i) = init
                      [k] = typeKernel ty
                      varname = mkView k
                      assign = SH_ExprWrapper upos $ AS_InfixOP epos AS_EQ
                                                       (mk_AS_Ident varname)
                                                       i
                   in [SH_I_ChangeState upos [assign]]
              | otherwise  = []
          h _ = []


-- NOTE kramer@acm.org reto -- we're using mkVar in here to generate st_R[r]
-- which really is knowledge otherwise limited to the TLACodeGen layer!
-- FIXME kramer@acm.org reto -- any way I could remove TLA knowledge from this
-- rewrite would be great (to be fair though, for timer handling I do the same
-- thing in that the RewriteTimer functions do a lot of st_ and st_GLOBAL
-- manipulation).
genLastActions :: SH_FL_Spec -> String -> [SH_Instr]
genLastActions spec name =
    let i0 = [SH_I_ChangeState upos
               [SH_ExprWrapper upos
                  (AS_InfixOP epos AS_EQ
                     (mk_AS_Ident "g_running")
                     (AS_Bool epos False))]]
        handlers = crashHandlingRolesFor spec name
        sends = map (\h ->
                     let dest = upper h -- FIXME kramer@acm.org reto -- ****
                         destE = AS_SetComprehension epos
                                   (AS_QBound1
                                     (mk_AS_Ident $
                                        "local_"++lower dest)
                                     (mk_AS_Ident $ dest))
                                   (AS_InfixOP epos AS_DOT
                                     (AS_InfixOP epos AS_FunApp
                                        (mk_AS_Ident $ mkVar dest)
                                        (AS_FunArgList epos
                                           [mk_AS_Ident $
                                              "local_"++lower dest]))
                                     (mk_AS_Ident "g_running"))
                      in SH_I_MsgSend1 upos (lower name)
                           True {- multi send -}
                           True {- LAST GASP msg, no append, set obuf to msg -}
                           (SH_ExprWrapper upos destE)
                           (mkCrashMsgName name dest)
                           [] -- use .sender to learn crashed instance
                    ) handlers
      in i0 ++ sends

predicateWhen :: AS_Expression -> [SH_RoleElement] -> [SH_RoleElement]
predicateWhen predicate l = everywhere (mkT (f predicate)) l
  where f p (SH_MsgHandler info ann r when label hook any from l) =
            SH_MsgHandler info ann r (addP p when) label hook any from l
        f p (SH_CallHandler info r when label args hook l) =
            SH_CallHandler info r (addP p when) label args hook l
        f p (SH_TimeoutHandler info r when id hook l) =
            SH_TimeoutHandler info r (addP p when) id hook l
        f p (SH_CrashHandler info ann r when n id hook l) =
            SH_CrashHandler info ann r (addP p when) n id hook l
        f p (SH_Every info r when period hook l) =
            SH_Every info r (addP p when) period hook l
        f p (SH_Once info r when label hook l) =
            SH_Once info r (addP p when) label hook l
        f _ x = x
        addP pred Nothing =
            Just $ SH_ExprWrapper upos pred
        addP pred (Just (SH_ExprWrapper _ p)) =
            Just $ SH_ExprWrapper upos (AS_LAND epos [pred, p])

-- FIXME kramer@acm.org reto -- perhaps make this configurable, to allow
-- the user to manage the statespace.
-- List the roles that are addressed by another role's crash handler, incl.
-- the roles that handle start events (i.e. have a "hook_do_start_X" extension)
-- FIXME kramer@acm.org reto -- rename function to indicate that it's not only
-- crashes, but also starts that matter here.
spontanousCrashers :: SH_FL_Spec -> [String]
spontanousCrashers spec =
       crashHandled spec -- for now all roles that have a crash handler
                         -- listening can fall over anytime
    ++ startExtension spec

crashableRoles :: SH_FL_Spec -> [String]
crashableRoles spec =
    nub $ (crashHandled spec) ++ (containsShutdown spec) ++
          (startExtension spec)

crashHandled :: SH_FL_Spec -> [String]
crashHandled spec = (everything (++) ([] `mkQ` f)) spec
    where f (SH_CrashHandler _ _ _enclrole _ rolename _ _ _) = [rolename]
          f _ = []

startExtension :: SH_FL_Spec -> [String]
startExtension spec = (everything (++) ([] `mkQ` f)) spec
    where f (SH_RoleDef _ name _ l) = if containsStartExtension0 name l /= []
                                      then [name]
                                      else []
          f _ = []
          containsStartExtension0 :: String -> [SH_RoleElement] -> [Bool]
          containsStartExtension0 r l = (everything (++) ([] `mkQ` (f r))) l
            where f r (SH_Extend_Hook _ _ l _) =
                      let a = any (\(SH_HookCallee _ h _) ->
                                    "hook_do_start_" ++ r == h)
                              l
                       in if a then [True] else []
                  f _ _ = []

containsShutdown :: SH_FL_Spec -> [String]
containsShutdown spec = (everything (++) ([] `mkQ` f)) spec
    where f (SH_RoleDef _ name _ l) = if containsShutdown0 l /= []
                                      then [name]
                                      else []
          f _ = []
          containsShutdown0 :: [SH_RoleElement] -> [Bool]
          containsShutdown0 l = (everything (++) ([] `mkQ` f)) l
            where f (SH_I_Shutdown _) = [True]
                  f _ = []

crashHandlingRolesFor :: SH_FL_Spec -> String -> [String]
crashHandlingRolesFor spec name = (everything (++) ([] `mkQ` f)) spec
    where f (SH_CrashHandler _ _ enclrole _ rolename _ _ _)
              | rolename == name = [enclrole]
          f _ = []

crashHandlersInRole :: String -> SH_FL_Spec -> [SH_RoleElement]
crashHandlersInRole name spec = (everything (++) ([] `mkQ` (f name))) spec
    where f name h@(SH_CrashHandler _ _ enclrole _ _rolename _ _ _)
              | enclrole == name = [h]
          f _ _ = []

mkCrashMsgName from to =
  "crash" ++ from ++ to -- avoid "_" in concat string due to ./sl and .tex's
                        -- interpretation as a math subscript.

prependIL :: [SH_Instr] -> [SH_GuardedInstrList] -> [SH_GuardedInstrList]
prependIL il l = map (prependIL0 il) l

prependIL0 :: [SH_Instr] -> SH_GuardedInstrList -> SH_GuardedInstrList
prependIL0 il ginstr =
    let SH_GuardedInstrList info guard label l = ginstr
     in SH_GuardedInstrList info guard label (il ++ l)

appendIL :: [SH_Instr] -> [SH_GuardedInstrList] -> [SH_GuardedInstrList]
appendIL il l = map (appendIL0 il) l

appendIL0 :: [SH_Instr] -> SH_GuardedInstrList -> SH_GuardedInstrList
appendIL0 il ginstr =
    let SH_GuardedInstrList info guard label l = ginstr
     in SH_GuardedInstrList info guard label (l ++ il)

lower = map toLower

-- capitalize the first letter
upper [c] = [toUpper c]
upper (x:xs) = (toUpper x : xs)

---- HELPER -------------------------------------------------------------------
mk_AS_Ident s = AS_Ident epos [] s

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos name line col = newPos name line col

upos = mkPos "foo" 0 0
epos = (upos, Nothing, Nothing)
