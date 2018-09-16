module RewriteDoMeanwhile(rewriteDoMeanwhile) where

import Data.Generics
import Syntax
import Flatten
import Debug.Trace(trace)
import Text.ParserCombinators.Parsec.Pos as PPos
import Language.TLAPlus.Syntax as TLASyntax
import RewriteLifecycle(predicateWhen)
import RewriteCont(label)

rewriteDoMeanwhile :: SH_FL_Spec -> SH_FL_Spec
rewriteDoMeanwhile = everywhere (mkT f)
  where f (SH_RoleDef info role vars l) =
            let (e, newH) = unzip $ map g l
                newH' = concat newH
             in SH_RoleDef info role vars (e ++ newH')
        f x = x

g :: SH_RoleElement -> (SH_RoleElement, [SH_RoleElement])
g h@(SH_MsgHandler info ann role when mtype lab any from gil) =
    let (gil', hndl) = rewriteDoMeanwhile0 (label h) gil
        in (SH_MsgHandler info ann role when mtype lab any from gil', hndl)
g h@(SH_CallHandler info role when lab args hook gil) =
    let (gil', hndl) = rewriteDoMeanwhile0 (label h) gil
        in (SH_CallHandler info role when lab args hook gil', hndl)
g h@(SH_TimeoutHandler info role when lab hook gil) =
    let (gil', hndl) = rewriteDoMeanwhile0 (label h) gil
        in (SH_TimeoutHandler info role when lab hook gil', hndl)
g h@(SH_CrashHandler info ann role when remote id hook gil) =
    let (gil', hndl) = rewriteDoMeanwhile0 (label h) gil
        in (SH_CrashHandler info ann role when remote id hook gil', hndl)
g h@(SH_Every info role when period hook gil) =
    let (gil', hndl) = rewriteDoMeanwhile0 (label h) gil
        in (SH_Every info role when period hook gil', hndl)
g h@(SH_Extend_Hook info role hookcalle gil) =
    let (gil', hndl) = rewriteDoMeanwhile0 (label h) gil
        in (SH_Extend_Hook info role hookcalle gil', hndl)
g h@(SH_Once info role when lab hook gil) =
    let (gil', hndl) = rewriteDoMeanwhile0 (label h) gil
        in (SH_Once info role when lab hook gil', hndl)
g x = (x, [])

rewriteDoMeanwhile0 :: String -> [SH_GuardedInstrList]
                    -> ([SH_GuardedInstrList], [SH_RoleElement])
rewriteDoMeanwhile0 roothandler gil =
    let (gil', handlers) = unzip $ map (f roothandler) gil
     in (gil', concat handlers)
  where f :: String -> SH_GuardedInstrList
          -> (SH_GuardedInstrList, [SH_RoleElement])
        f roothandler (SH_GuardedInstrList info guard hooks il) =
            let (il', hndl) = glueTogether roothandler il
             in (SH_GuardedInstrList info guard hooks il', hndl)

glueTogether :: String -> [SH_Instr] -> ([SH_Instr], [SH_RoleElement])
glueTogether roothandler il =
    let (il', h) = unzip (map (glueTogether0 roothandler) il)
     in (concat il', concat h)

glueTogether0 :: String -> SH_Instr -> ([SH_Instr], [SH_RoleElement])
glueTogether0 roothandler (SH_I_DoMeanwhile _ il handlers) =
    let (labels, il') = labelAwait il
        e = mkOR (map (\l -> mk_AS_Ident $ roothandler ++ "@" ++ l) labels)
        handlers' = predicateWhen e handlers
     in (il', handlers')
glueTogether0 _ i = ([i], [])

labelAwait :: [SH_Instr] -> ([String], [SH_Instr])
labelAwait l =
    let ids = numberAwaits l
        (labels, is) = unzip $ map labelAwait0 (zip ids l)
     in (concat labels, is) -- concat to drop [] entries
  where labelAwait0 :: (Int, SH_Instr) -> ([String], SH_Instr)
        labelAwait0 (i, SH_I_Await info handler) =
            case handler of
              SH_MsgHandler info ann role when mtype hook any from gil ->
                ([mkL i], SH_I_Await info $
                            SH_MsgHandler info ann role when mtype
                              (combHook hook (mkL i)) any from gil)
              SH_CallHandler info role when label args hook gil ->
                ([mkL i], SH_I_Await info $
                            SH_CallHandler info role when label args
                              (combHook hook (mkL i)) gil)
              SH_TimeoutHandler info role when label hook gil ->
                ([mkL i], SH_I_Await info $
                            SH_TimeoutHandler info role when label
                              (combHook hook (mkL i)) gil)
              SH_CrashHandler info ann role when remote id hook gil ->
                ([mkL i], SH_I_Await info $
                            SH_CrashHandler info ann role when remote id
                              (combHook hook (mkL i)) gil)
              SH_Every info role when period hook gil ->
                ([mkL i], SH_I_Await info $
                            SH_Every info role when period
                              (combHook hook (mkL i)) gil)
              SH_Once info role when label hook gil ->
                ([mkL i], SH_I_Await info $
                            SH_Once info role when label
                              (combHook hook (mkL i)) gil)
        labelAwait0 (_, x) = ([], x)
        combHook Nothing s  = Just [SH_HookCaller upos s []]
        combHook (Just l) s = Just (l ++ [SH_HookCaller upos s []])
        mkL i = "wp_" ++ show i
        numberAwaits l = numberAwaits0 0 l []
        numberAwaits0 n [] acc = acc
        numberAwaits0 n [(SH_I_Await _ _)] acc = acc ++ [n+1]
        numberAwaits0 n [_] acc = acc ++ [n]
        numberAwaits0 n ((SH_I_Await _ _):rest) acc =
            numberAwaits0 (n+1) rest (acc ++ [n+1])
        numberAwaits0 n (_:rest) acc =
            numberAwaits0 n rest (acc ++ [n])

mkOR [e] = e
mkOR [a,b] = AS_InfixOP epos AS_OR a b
mkOR l = AS_LOR epos l

---- HELPER -------------------------------------------------------------------
mk_AS_Ident s = AS_Ident epos [] s

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos name line col = newPos name line col

upos = mkPos "foo" 0 0
epos = (upos, Nothing, Nothing)
