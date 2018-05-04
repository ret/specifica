module RewriteCont(rewriteCont,
                   typeDefaultValue, -- helper for RewriteRewriteStateInit
                   beautifyLAND,
                   label
                  ) where

import Data.Generics
import Data.List(elemIndex)
import Syntax
import Flatten
import Debug.Trace(trace)
import RewriteLifecycle(appendIL, predicateWhen)
import RewriteTimer(every)
import TLACodeGen(mk_AS_Type)
import Text.ParserCombinators.Parsec.Pos as PPos
import Language.TLAPlus.Syntax as TLASyntax

rewriteCont :: SH_FL_Spec -> SH_FL_Spec
rewriteCont = beautifyLAND
            . everywhere (mkT f)
            . rewriteRewind
             . rewritePCLocationGuards -- do this _before_ await is broken out
                                      -- since that step looses the @hooks
                                      -- on the await statements
    where f roledef@(SH_RoleDef _ rname vars elems) =
              let states = extractStates elems
                  elems' = rewriteInlineStates elems
                  gen = concat $ map splitSections elems'
                  elems'' = removeHandlersWithAWAIT elems'
               in SH_RoleDef upos rname vars $ states ++ elems'' ++ gen
          f x = x

-- FIXME kramer@acm.org reto -- must include SH_Extend_Hook since at the
-- point we do rewriteCont, the extension blocks have not been merged.
removeHandlersWithAWAIT :: [SH_RoleElement] -> [SH_RoleElement]
removeHandlersWithAWAIT = concat . map f
  where f h@(SH_MsgHandler _ _ _ _ _ _ _ _ l) = if hasAwait l then [] else [h]
        f h@(SH_CallHandler _ _ _ _ _ _ l)  = if hasAwait l then [] else [h]
        f h@(SH_TimeoutHandler _ _ _ _ _ l) = if hasAwait l then [] else [h]
        f h@(SH_CrashHandler _ _ _ _ _ _ _ l) = if hasAwait l then [] else [h]
        f h@(SH_Every _ _ _ _ _ l)          = if hasAwait l then [] else [h]
        f h@(SH_Once _ _ _ _ _ l)           = if hasAwait l then [] else [h]
        f x = [x]


-- FIXME kramer@acm.org reto -- must include SH_Extend_Hook since at the
-- point we do rewriteCont, the extension blocks have not been merged.
splitSections :: SH_RoleElement -> [SH_RoleElement]
splitSections h@(SH_MsgHandler _ _ _ _ _ _ _ _ l) = split0 h l
splitSections h@(SH_CallHandler _ _ _ _ _ _ l)  = split0 h l
splitSections h@(SH_TimeoutHandler _ _ _ _ _ l) = split0 h l
splitSections h@(SH_CrashHandler _ _ _ _ _ _ _ l) = split0 h l
splitSections h@(SH_Every _ _ _ _ _ l)          = split0 h l
splitSections h@(SH_Once _ _ _ _ _ l)           = split0 h l
splitSections _ = [] -- all other elements (e.g. STATE) are present in elems'
                     -- already, avoid duplicating!

split0 ha l
  | hasAwait l =
      let pc = label ha
          rootG = AS_InfixOP epos AS_EQ
                    (mk_AS_Ident $ mkPC pc)
                    (AS_Num epos 0)
          (l', newH) = splitGIL pc l
          ha' = replGILs ha l'
          seqState = SH_State upos False
                       (SH_Ty_UserDef upos
                          ("(0.." ++ show (length newH) ++ ")"),
                          mkPC pc)
                       (Just $ SH_ExprWrapper upos (AS_Num epos 0))
       in predicateWhen rootG [ha'] ++ [seqState] ++ newH
  | otherwise = [] -- included in rewriteCont/f already, do not duplicate

replGILs (SH_MsgHandler info ann role when mtype label any from _) l =
    SH_MsgHandler info ann role when mtype label any from l
replGILs (SH_CallHandler info role when label args hook _) l =
    SH_CallHandler info role when label args hook l
replGILs (SH_TimeoutHandler info role when label hook _) l =
    SH_TimeoutHandler info role when label hook l
replGILs (SH_CrashHandler info ann role when remote id hook _) l =
    SH_CrashHandler info ann role when remote id hook l
replGILs (SH_Every info role when period hook _) l =
    SH_Every info role when period hook l
replGILs (SH_Once info role when label hook _) l =
    SH_Once info role when label hook l

splitGIL :: String -> [SH_GuardedInstrList]
         -> ([SH_GuardedInstrList], [SH_RoleElement])
splitGIL pc l =
  let parts = map (splitGIL0 pc) l
      (gils', res) = roll parts 0 (f pc) []
   in (gils', res)
  where splitGIL0 :: String -> SH_GuardedInstrList
                  -> (SH_GuardedInstrList, [SH_Instr])
        splitGIL0 pc gil@(SH_GuardedInstrList _ guard hooks l)
            | any isAwait l =
                let l1 = takeWhile (not . isAwait) l
                    l2 = dropWhile (not . isAwait) l
                 in (SH_GuardedInstrList upos guard hooks l1, l2)
            | otherwise = (gil, [])
        isAwait (SH_I_Await _ _) = True
        isAwait _ = False
        f :: String -> Int -> (SH_GuardedInstrList, [SH_Instr])
          -> (Int, SH_GuardedInstrList, [SH_RoleElement])
        f pc i (gil, il)
            | il /= [] = let sections = chop isAwait il []
                             rels = map
                                      (g pc (i+length sections))
                                      (zip [i+1..1+i+length sections] sections)
                             goto = SH_I_ChangeState upos
                                      [SH_ExprWrapper upos
                                        (AS_InfixOP epos AS_EQ
                                          (mk_AS_Ident $ mkPC pc)
                                          (AS_Num epos (i+1)))]
                             SH_GuardedInstrList info guard hooks l = gil
                             gil' = SH_GuardedInstrList info guard hooks
                                      (l ++ [goto])
                          in (i+length sections,
                              gil', rels)
            | il == [] = (i, gil, [])
           where g :: String -> Int -> (Int, Section SH_Instr)
                  -> SH_RoleElement
                 g root last (n,(await, il)) =
                   let SH_I_Await _ handler = await
                       guard = AS_InfixOP epos AS_EQ
                                 (mk_AS_Ident $ mkPC root)
                                 (AS_Num epos n)
                       [handler'] = predicateWhen guard [handler]
                       nextPC = if n == last then 0 else n + 1
                       upSeq = SH_I_ChangeState upos
                                 [SH_ExprWrapper upos
                                    (AS_InfixOP epos AS_EQ
                                       (mk_AS_Ident $ mkPC root)
                                       (AS_Num epos nextPC))]
                    in injectIL_H handler' (il ++ [upSeq])



roll :: [(SH_GuardedInstrList, [SH_Instr])] -- parts
     -> Int
     -> (Int -> (SH_GuardedInstrList, [SH_Instr])
         -> (Int, SH_GuardedInstrList, [SH_RoleElement]) ) -- f
     -> [(SH_GuardedInstrList, [SH_RoleElement])] -- acc
     -> ([SH_GuardedInstrList], [SH_RoleElement])
roll []                i f acc =
    let (gils, rels) = unzip acc
     in (gils, concat rels)
roll ((gil, il):rest) i f acc =
    let (i', gil', rels) = f i (gil, il)
        acc' = acc ++ [(gil', rels)]
     in roll rest i' f acc'

hasAwait :: [SH_GuardedInstrList] -> Bool
hasAwait = any hasAwait0
  where hasAwait0 :: SH_GuardedInstrList -> Bool
        hasAwait0 gil = [] /= (everything (++) ([] `mkQ` f)) gil
          where f (SH_I_Await _ _) = [True]
                f _ = []

splitSectionsH :: SH_RoleElement -> SH_GuardedInstrList -> [SH_RoleElement]
splitSectionsH h gil =
  if hasAwait [gil]
  then let SH_GuardedInstrList _ _guard _hooks il = gil
           -- root handler h is wrapped in an await for uniformity
           sections = chop test ([SH_I_Await upos (injectIL_H h [])] ++ il) []
           handlers = map (f (label h) -- name of the root handler for PC_...
                             (length sections - 1))
                          (zip [0..length sections] sections)
           sequencerState = SH_State upos False
                              (SH_Ty_UserDef upos
                               ("(0.." ++ show (length sections - 1) ++ ")"),
                               mkPC (label h))
                              (Just $ SH_ExprWrapper upos (AS_Num epos 0))
        in [sequencerState] ++ handlers
  else [] -- do not add handlers, keep existing one
  where f :: String -> Int -> (Int, Section SH_Instr) -> SH_RoleElement
        f root last (n,(await, il)) =
            let SH_I_Await _ handler = await
                guard = AS_InfixOP epos AS_EQ
                          (mk_AS_Ident $ mkPC root)
                          (AS_Num epos n)
                [handler'] = predicateWhen guard [handler]
                nextPC = if n == last then 0 else n + 1
                upSeq = SH_I_ChangeState upos
                          [SH_ExprWrapper upos
                           (AS_InfixOP epos AS_EQ
                            (mk_AS_Ident $ mkPC root)
                            (AS_Num epos nextPC))]
             in injectIL_H handler' (il ++ [upSeq])
        test (SH_I_Await _ _) = True
        test _ = False

type Section a = (a, [a])
-- assumes (test $ head l), i.e. the first element in the list is a header
chop :: Eq a => (a -> Bool) -> [a] -> [Section a] -> [Section a]
chop _ [] _ = []
chop test l acc = case chop0 test (head l) (tail l) [] of
                    (section, [])    -> acc ++ [section]
                    (section, rest)  -> chop test rest (acc ++ [section])

chop0 :: Eq a => (a -> Bool) -> a -> [a] -> [a] -> (Section a, [a])
chop0 test head []         acc             = ((head, acc), [])
chop0 test head l@(h:rest) acc | test h    = ((head, acc), l)
                               | otherwise = chop0 test head rest (acc ++ [h])

-- FIXME kramer@acm.org reto -- must include SH_Extend_Hook since at the
-- point we do rewriteCont, the extension blocks have not been merged.
injectIL_H :: SH_RoleElement -> [SH_Instr] -> SH_RoleElement
-- these are the clause used for the original handler, that was turned into
-- one with an empty gil
injectIL_H (SH_MsgHandler info ann role when mtype label any from []) l =
    SH_MsgHandler info ann role when mtype label any from
                      [SH_GuardedInstrList upos Nothing Nothing l]
injectIL_H (SH_CallHandler info role when label args hook []) l =
    SH_CallHandler info role when label args hook
                       [SH_GuardedInstrList upos Nothing Nothing l]
injectIL_H (SH_TimeoutHandler info role when label hook []) l =
    SH_TimeoutHandler info role when label hook
                          [SH_GuardedInstrList upos Nothing Nothing l]
injectIL_H (SH_CrashHandler info ann role when remote id hook []) l =
    SH_CrashHandler info ann role when remote id hook
                        [SH_GuardedInstrList upos Nothing Nothing l]
injectIL_H (SH_Every info role when period hook []) l =
    SH_Every info role when period hook
                 [SH_GuardedInstrList upos Nothing Nothing l]
injectIL_H (SH_Once info role when label hook []) l =
    SH_Once info role when label hook
                [SH_GuardedInstrList upos Nothing Nothing l]
-- these are the clauses invoked for _AWAIT_ MSG ... cases
injectIL_H (SH_MsgHandler info ann role when mtype label any from [gil]) l =
    SH_MsgHandler info ann role when mtype label any from [replaceIL_G gil l]
injectIL_H (SH_CallHandler info role when label args hook [gil]) l =
    SH_CallHandler info role when label args hook [replaceIL_G gil l]
injectIL_H (SH_TimeoutHandler info role when label hook [gil]) l =
    SH_TimeoutHandler info role when label hook [replaceIL_G gil l]
injectIL_H (SH_CrashHandler info ann role when remote id hook [gil]) l =
    SH_CrashHandler info ann role when remote id hook [replaceIL_G gil l]
injectIL_H (SH_Every info role when period hook [gil]) l =
    SH_Every info role when period hook [replaceIL_G gil l]
injectIL_H (SH_Once info role when label hook [gil]) l =
    SH_Once info role when label hook [replaceIL_G gil l]

replaceIL_G :: SH_GuardedInstrList -> [SH_Instr] -> SH_GuardedInstrList
replaceIL_G (SH_GuardedInstrList info guard hook il) l =
    SH_GuardedInstrList info guard hook l -- replace il with l

beautifyLAND :: SH_FL_Spec -> SH_FL_Spec
beautifyLAND = everywhere (mkT f)
    where f (AS_LAND info l) = AS_LAND info $ flatten l
          f x = x
          flatten [] = []
          flatten ((AS_LAND _ l):rest) = flatten $ l ++ rest
          flatten (h:rest) = h : flatten rest

rewriteRewind :: SH_FL_Spec -> SH_FL_Spec
rewriteRewind spec = everywhere (mkT (f spec)) spec
    where f spec (SH_I_Rewind _ role id loc) =
              let pc = case loc of
                         Nothing -> "0" -- reset to start
                         Just loc' -> idxOfAwaitIn spec role loc'
               in SH_I_ChangeState upos
                      [SH_ExprWrapper upos
                       (AS_InfixOP epos AS_EQ
                        (mk_AS_Ident $ mkPC id)
                        (mk_AS_Ident pc))]
          f _ x = x

-- extract the inline STATE declarations from all instruction lists
extractStates :: [SH_RoleElement] -> [SH_RoleElement]
extractStates = everything (++) ([] `mkQ` f)
  where f (SH_I_State _p s) = [initDefault s]
        f _ = []
        -- initialize the state to the default value for the type
        initDefault (SH_State p1 per v@(ty, var)
                       (Just (SH_ExprWrapper p2 init))) =
            SH_State p1 per v (Just (SH_ExprWrapper p2
                                         (typeDefaultValue ty)))

typeDefaultValue (SH_Ty_UserDef _ s) = typeDefaultValueUserDef s
typeDefaultValue (SH_Ty_UserDefOrNIL _ t) = mk_AS_Ident "NIL"
typeDefaultValue (SH_Ty_Expr _ t) = mk_AS_Ident "Outsch_need_type_inference"
typeDefaultValue (SH_Ty_SetOf _ t) = AS_DiscreteSet epos []
typeDefaultValue (SH_Ty_SeqOf _ t) = AS_Tuple epos []
typeDefaultValue (SH_Ty_PairOf _ tA tB) = AS_Tuple epos [ typeDefaultValue tA
                                                        , typeDefaultValue tB ]
typeDefaultValue (SH_Ty_Map _ tA tB) = AS_QuantifierBoundFunction epos
                                       [AS_QBoundN [mk_AS_Ident "local_x"]
                                        (mk_AS_Type tA)]
                                       (typeDefaultValue tB)
typeDefaultValue (SH_Ty_Enum _ l) = mk_AS_Ident (show $ head l)
-- FIXME kramer@acm.org reto -- add SH_Ty_Union support

typeDefaultValueUserDef "BOOLEAN" = AS_Bool epos False
typeDefaultValueUserDef "Nat" = AS_Num epos 0
-- turn this into TypeDefaultValue("SomeType") such that I can let users write
-- a TLA { TypeDefaultValue("SomeType") == ... }
-- Work around for lack of type inference, see prim_stateinit
typeDefaultValueUserDef unknown =
    AS_OpApp epos (mk_AS_Ident "TypeDefaultValue") [mk_AS_Ident $ show unknown]

rewriteInlineStates :: [SH_RoleElement] -> [SH_RoleElement]
rewriteInlineStates = everywhere (mkT f)
  where f (SH_I_State _ (SH_State _ _ (ty, v)
                             (Just (SH_ExprWrapper _ init)))) =
            SH_I_ChangeState upos
              [SH_ExprWrapper upos
               (AS_InfixOP epos AS_EQ
                (mk_AS_Ident v)
                init)]
        f x = x

mkPC id = "g_pc" ++ "_" ++ id

label :: SH_RoleElement -> String
label (SH_MsgHandler _ _ _ _ label _ _ _ _) = label
label (SH_CallHandler _ _ _ label _ _ _)  = label
label (SH_TimeoutHandler _ _ _ id _ _)    = id
label (SH_CrashHandler _ _ _ _ _ id _ _)    = id
label (SH_Every _ _ _ period _ _)         = every period
label (SH_Once _ _ _ label _ _)           = label

-- FIXME kramer@acm.org reto -- also rewrite guards in GILs!
rewritePCLocationGuards :: SH_FL_Spec -> SH_FL_Spec
rewritePCLocationGuards spec = everywhere (mkT (f spec)) spec
  where f spec (SH_MsgHandler info ann role when mtype label any from gil) =
            SH_MsgHandler info ann role (rewritePCLoc spec role when)
                          mtype label any from (rewritePCLocGIL spec role gil)
        f spec (SH_CallHandler info role when label args hook gil) =
            SH_CallHandler info role (rewritePCLoc spec role when)
                           label args hook (rewritePCLocGIL spec role gil)
        f spec (SH_TimeoutHandler info role when label hook gil) =
            SH_TimeoutHandler info role (rewritePCLoc spec role when)
                              label hook (rewritePCLocGIL spec role gil)
        f spec (SH_CrashHandler info ann role when remote id hook gil) =
            SH_CrashHandler info ann role (rewritePCLoc spec role when)
                            remote id hook (rewritePCLocGIL spec role gil)
        f spec (SH_Every info role when period hook gil) =
            SH_Every info role (rewritePCLoc spec role when)
                     period hook (rewritePCLocGIL spec role gil)
        f spec (SH_Extend_Hook info role hookcalle gil) =
            SH_Extend_Hook info role hookcalle (rewritePCLocGIL spec role gil)
        f spec (SH_Once info role when label hook gil) =
            SH_Once info role (rewritePCLoc spec role when)
                    label hook (rewritePCLocGIL spec role gil)
        f _ x = x

rewritePCLoc :: SH_FL_Spec -> String -> (Maybe SH_ExprWrapper)
                    -> (Maybe SH_ExprWrapper)
rewritePCLoc spec role Nothing = Nothing
rewritePCLoc spec role guard   = everywhere (mkT (f spec role)) guard
  where f spec role i@(AS_Ident _ _ name) =
            case break (=='@') name of
              (_,[]) -> i -- no @ present
              (root, ('@':label)) ->
                  AS_InfixOP epos AS_EQ
                    (mk_AS_Ident $ mkPC root)
                    (mk_AS_Ident $ idxOfAwaitIn spec role label)
        f _ _ x = x

rewritePCLocGIL :: SH_FL_Spec -> String -> [SH_GuardedInstrList]
                -> [SH_GuardedInstrList]
rewritePCLocGIL spec role l = everywhere (mkT (f spec role)) l
  where f spec role (SH_GuardedInstrList info guard hook l) =
            SH_GuardedInstrList info (rewritePCLoc spec role guard) hook l

-- FIXME kramer@acm.org reto -- make role specific, i.e. allow same hndlLabel
-- in different roles, don't confuse them!!
-- FIXME kramer@acm.org reto -- return type should be Int, is string to allow
-- really lame error handling in case the label used in the guard isn't found
-- in the spec.
idxOfAwaitIn :: SH_FL_Spec -> String -> String -> String
idxOfAwaitIn spec role "init" = "0" -- convention
idxOfAwaitIn spec role hndlLabel = -- lame error handling, unknown label == -1
    let allGILs = everything (++) ([] `mkQ` f) spec
        bl = map (\l -> map (g hndlLabel) l) allGILs
        fl = filter (\l -> not $ all (\x -> case x of
                                              Nothing -> True
                                              Just _ -> False) l) bl
     in if fl == []
        -- FIXME kramer@acm.org reto -- really lame error handling!!
        then "error-loc-not-found=" ++ hndlLabel
        else let jl = filter (/= Nothing) $ head fl
              in case elemIndex (Just True) jl of
                   Nothing -> "error-loc-not-found=" ++ hndlLabel
                   Just i -> show (1+i)
  where f (SH_GuardedInstrList _ _ _ l) = [l]
        -- an await will result in Just True
        --   if @label matches, Just False otherwise
        -- any statement other than await, creates Nothing
        g :: String -> SH_Instr -> Maybe Bool
        g hndlLabel a@(SH_I_Await _ h) = Just $ isHandlerLabeled hndlLabel h
        g _ _ = Nothing
        isHandlerLabeled ho (SH_MsgHandler _ _ _ _ _ h _ _ _) = hookEq ho h
        isHandlerLabeled ho (SH_CallHandler _ _ _ _ _ h _)  = hookEq ho h
        isHandlerLabeled ho (SH_TimeoutHandler _ _ _ _ h _) = hookEq ho h
        isHandlerLabeled ho (SH_CrashHandler _ _ _ _ _ _ h _) = hookEq ho h
        isHandlerLabeled ho (SH_Every _ _ _ _ h _)          = hookEq ho h
        isHandlerLabeled ho (SH_Once _ _ _ _ h _)           = hookEq ho h
        hookEq s Nothing = False
        hookEq s (Just l) = any (\(SH_HookCaller _ name _) -> (name == s)) l

---- HELPER -------------------------------------------------------------------
mk_AS_Ident s = AS_Ident epos [] s

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos name line col = newPos name line col

upos = mkPos "foo" 0 0
epos = (upos, Nothing, Nothing)
