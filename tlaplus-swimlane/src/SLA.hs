module SLA where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language( emptyDef )

import Language.TLAPlus.Parser( tlaspec, cfgspec, mkState )
import Language.TLAPlus.Syntax
import Language.TLAPlus.Pretty( prettyPrintAS, prettyPrintVA,
                                prettyPrintVATeX,
                                prettyPrintE, prettyPrintCFG )
import Language.TLAPlus.Eval
import TraceReader
import Parser (Expr(..), Ident(..))
import qualified Data.Map as Map (map, toList, fromList, keys)
import Data.Map as Map (lookup)
import qualified Data.Set as Set (map, toList)

import Debug.Trace as Trace
import System.IO (hPutStrLn, stderr)
import Control.Exception (catch)
import System.IO.Error (ioError, isDoesNotExistError)
import System.Environment
import System.Exit

readSLA :: String -> IO (Either String (Maybe StateAnnFun, Maybe SLAFun))
readSLA fname =
    do{ let cfgname = case reverse fname of
                      ('a':'l':'t':'.':fn) -> reverse fn ++ ".cfg"
                      ('a':'l':'s':'.':fn) -> reverse fn ++ ".cfg"
                      _ -> fname ++ ".cfg"
      ; let slaname = case reverse fname of
                      ('a':'l':'s':'.':fn) -> reverse fn ++ ".sla"
                      ('a':'l':'t':'.':fn) -> reverse fn ++ ".sla"
                      _ -> fname ++ ".sla"
      ; cfg <- catch (readFile cfgname) errorHandler -- cfg is optional
      ; case (runParser cfgspec mkState cfgname cfg) of
          Left err -> do{ let err' = "cfg configuration parse error at " ++
                                     (show err)
                        ; return $ Left err'
                        }
          Right cfg  ->
            do{ -- hPutStrLn stderr $ prettyPrintCFG cfg
              ; readTLA slaname >>= \res -> case res of
                  Left err ->
                    do{ let err' = "Swimlane annotation parse error at " ++
                                   (show err)
                      ; return $ Left err'
                      }
                  Right tlaspecs ->
                    do{ -- hPutStrLn stderr (show tlaspec)
                      ; let a = evalReturnEnv (reverse tlaspecs) cfg
                      ; case a of
                          Left err -> return $ Left $ ppError err
                          -- FIXME only one ASSUME clause, introduce
                          -- "EVAL" keyword in SLA file, or better use
                          -- SLA == ...
                          Right (env, [(VA_Rec m)]) ->
                            case Map.lookup (VA_String "messages") m of
                              Just g@(VA_FunctionDef _ _ _ _) ->
                                let slafun = wrapSLAFun env g
                                 in case Map.lookup
                                           (VA_String "state_transitions")
                                           m of
                                      Just f@(VA_FunctionDef _info _head
                                                _qbounds _expr) ->
                                        let sf = wrapStateFun env f
                                         in return $ Right $
                                              (Just sf, Just slafun)
                                      Nothing ->
                                        return $ Right $
                                          (Nothing, Just slafun)
                                      _ -> error "unspecified"
                              Nothing ->
                                 case Map.lookup
                                        (VA_String "state_transitions")
                                        m of
                                   Just f@(VA_FunctionDef _info _head
                                              _qbounds _expr) ->
                                     let sf = wrapStateFun env f
                                      in return $ Right $ (Just sf, Nothing)
                                   Nothing ->
                                     return $ Right $ (Nothing, Nothing)
                                   _ -> error "unspecified"
                              _ -> error "unspecified"
                          _ -> error "unspecified"
                      }
              }
      }
    where errorHandler e = if isDoesNotExistError e
                           then return "" -- default is empty
                           else ioError e -- rethrow

wrapStateFun :: Env -> VA_Value -> StateAnnFun
wrapStateFun globalenv (VA_FunctionDef _info _head _qbounds expr) =
    -- FIXME must enforce qbounds!
    \state ->
      let va_state = convertEtoVA state -- record
       in case bind globalenv (ident "transition", va_state) >>= \env ->
            evalE env expr of
              Right (VA_Rec m) ->
                  let label = extractElem m "label" Label
                      labelfont = extractElem m "font" LabelFont
                      style = extractElemString m "style" Style
                      hidediff = extractElemSet m "hidediff" HideDiff
                   in Just $ label ++ labelfont ++ style ++ hidediff
              Right other ->
                  Trace.trace ("Expect record, found "++(prettyPrintVA other))
                       Nothing
              Left err ->
                Trace.trace ("ERROR evaluating state decoration function: "++
                             show err) Nothing
wrapStateFun _ _ = error "undefined"

wrapSLAFun :: Env -> VA_Value -> SLAFun
wrapSLAFun globalenv (VA_FunctionDef _info _head _qbounds expr) =
    -- FIXME must enforce qbounds!
    \statelist ->
      let va_statelist = VA_Seq $ map
                           (\(i,e) -> VA_Rec $ Map.fromList
                                        [(VA_String "idx", VA_Int i),
                                         (VA_String "state", convertEtoVA e)])
                           statelist
          -- FIXME create a record [state_idx: Int, state: State]
       in case bind globalenv (ident "states", va_statelist) >>= \env ->
            evalE env expr of
              Right (VA_Set recset) ->
                let l = concat $
                        map (\r -> case r of
                                     VA_Seq [] -> [] -- <<>> means no line
                                     _ -> conv r)
                          (Set.toList recset)
                 in l
              Left err ->
                Trace.trace ("ERROR evaluating sla function "++show err) []
              other ->
                Trace.trace ("UNEXPECTED TYPE evaluating sla function "++
                     show other) []

  where conv :: VA_Value -> [SLAMsg]
        conv (VA_Rec m) =
          let from = case Map.lookup (VA_String "from") m of
                       -- NOTE from/to are atoms, not strings!
                       Just (VA_Rec m) ->
                         case Map.lookup (VA_String "agent") m of
                           Just (VA_Atom from) -> from
                           o -> Trace.trace ("EXPECTED 'agent', found "++
                                             show o++", in "++
                                             prettyPrintVA (VA_Rec m) ) []
                       o -> Trace.trace ("EXPECTED 'from', found "++
                                         show o++", in "++
                                         prettyPrintVA (VA_Rec m) ) []
              i    = case Map.lookup (VA_String "from") m of
                       -- NOTE from/to are atoms, not strings!
                       Just (VA_Rec m) ->
                         case Map.lookup (VA_String "state") m of
                           Just (VA_Int i) -> i
                           o -> Trace.trace ("EXPECTED 'state', found "++
                                             show o++", in "++
                                             prettyPrintVA (VA_Rec m) ) 0
                       o -> Trace.trace ("EXPECTED 'from', found "++
                                         show o++", in "++
                                         prettyPrintVA (VA_Rec m) ) 0
              to   = case Map.lookup (VA_String "to") m of
                       -- NOTE from/to are atoms, not strings!
                       Just (VA_Rec m) ->
                         case Map.lookup (VA_String "agent") m of
                           Just (VA_Atom from) -> from
                           o -> Trace.trace ("EXPECTED 'agent', found "++
                                             show o++", in "++
                                             prettyPrintVA (VA_Rec m) ) []
                       o -> Trace.trace ("EXPECTED 'to', found "++
                                         show o++", in "++
                                         prettyPrintVA (VA_Rec m) ) []
              j    = case Map.lookup (VA_String "to") m of
                       -- NOTE from/to are atoms, not strings!
                       Just (VA_Rec m) ->
                         case Map.lookup (VA_String "state") m of
                           Just (VA_Int i) -> i
                           o -> Trace.trace ("EXPECTED 'state', found "++
                                             show o++", in "++
                                             prettyPrintVA (VA_Rec m) ) 0
                       o -> Trace.trace ("EXPECTED 'to', found "++
                                         show o++", in "++
                                         prettyPrintVA (VA_Rec m) ) 0
              style = extractElem m "style" Style
              color = extractElem m "color" Color
              label = extractElem m "label" Label
              labelfont = extractElem m "font" LabelFont
              tiplabel = extractElem m "tiplabel" TipLabel
              tiplabelfont = extractElem m "tipfont" TipLabelFont
              decoration = color ++ style ++
                           label ++ labelfont ++
                           tiplabel ++ tiplabelfont
           in [((i, from), (j, to), decoration)]
        conv other = Trace.trace ("ERROR: expected record, but got "++
                                  prettyPrintVA other) []
wrapSLAFun _ _ = error "unspecified"

readTLA :: String -> IO (Either ParseError [AS_Spec])
readTLA fname =
    do{ tla <- readFile fname
      ; case runParser tlaspec mkState fname tla of
          Left err -> return $ Left $ err
          Right tlaspec  ->
            let (AS_ExtendDecl _info l) = extendDecl tlaspec
                l' = filter (\s -> -- FIXME list all of the "std modules"
                       not (s `elem` ["TLC", "Naturals", "Integers",
                                      "FiniteSets", "Sequences"])) l
                l'' = map (\s -> s++".tla") l' -- extended names are .tla
             in mapM readTLA l'' >>= \as ->
               let as' = filter (\case
                                    Left err -> Trace.trace
                                                ("[error] "++(show err))
                                                False
                                    Right _ -> True) as
                in return $
                     Right ([tlaspec] ++ (concat $ map (\(Right t) -> t) as'))
      }

extractElem m f t = case Map.lookup (VA_String f) m of
                       -- NOTE label can be a
                       -- string, record or tuple
                       Just (VA_String label) -> [t label]
                       Just m@(VA_Rec _) -> [t (prettyPrintVATeX m)]
                       Just m@(VA_Map _) -> [t (prettyPrintVATeX m)]
                       Just (VA_Seq [VA_String sep, VA_Seq l]) ->
                         [t $ join sep (map (\v ->
                            case v of
                              VA_String s -> s -- hide double quotes
                              _ -> prettyPrintVATeX v) l)]
                       Just other -> Trace.trace
                         ("Warning, unknow result type "++show other) []
                       Nothing -> []

extractElemSet m f t = case Map.lookup (VA_String f) m of
                         Just (VA_Set s) -> -- Set of String expected!
                             [t $ map (\(VA_String s) -> s) (Set.toList s)]
                         Just other -> Trace.trace ("Warning, unknow result "++
                                                    "type (set expected) "++
                                                    show other) []
                         Nothing -> []

extractElemString m f t =
    case Map.lookup (VA_String f) m of
      Just (VA_String s) -> [t s]
      Just (VA_Seq []) -> [t ""]
      Just other -> Trace.trace ("Warning, unknow result type "++
                                 "(string or <<>> expected) "++
                                 show other) []
      Nothing -> []

ident s = AS_Ident (mkDummyInfo "") [] s

convertEtoVA :: Expr -> VA_Value
convertEtoVA (RecE m) =
    let l = map (\(k,v) -> (convertItoVA k, convertEtoVA v)) $ Map.toList m
     in VA_Rec $ Map.fromList l
convertEtoVA (MapE m) =
    let l = map (\(k,v) -> (convertEtoVA k, convertEtoVA v)) $ Map.toList m
     in VA_Map $ Map.fromList l
convertEtoVA (SetE s) = VA_Set $ Set.map (\e -> convertEtoVA e) s
convertEtoVA (SeqE l) = VA_Seq $ map (\e -> convertEtoVA e) l
convertEtoVA (IntE i) = VA_Int i
convertEtoVA (StrE s) = VA_String s
convertEtoVA (AtomE a) = case a of
                           "TRUE" -> VA_Bool True
                           "FALSE" -> VA_Bool False
                           _ -> VA_Atom a

convertItoVA :: Ident -> VA_Value
convertItoVA (Ident s) = VA_String s

join _sep []  = ""
join _sep [s] = s
join sep l   = let rl = reverse l
                in foldl (\e acc -> acc++sep++e) (head2 rl) (tail rl)

head2 [] = error "!"
head2 l = head l
