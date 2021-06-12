-- See:
--   http://dev.stephendiehl.com/hask/#quasiquotation
--   https://markkarpov.com/tutorial/th.html#example-2-creating-refined-values-at-compile-time
--   https://www.schoolofhaskell.com/user/marcin/quasiquotation-101

{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Language.TLAPlus.Quasiquote(
  tla,
  tla_e,
  tla_v,
  tla_u,
  tla_s
) where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           Language.TLAPlus.Eval
import           Language.TLAPlus.Parser           (expression, mkState,
                                                    tlaspec, unit1)
import           Language.TLAPlus.Pretty           (prettyPrintE, prettyPrintVA)
import           Language.TLAPlus.Syntax
import           Text.ParserCombinators.Parsec     (ParseError, runParser)
import           Text.ParserCombinators.Parsec.Pos as PPos

import           Data.Generics.Aliases             (extQ)

import           Data.Map                          as Map hiding (map)
import           Data.Set                          as Set hiding (map)

instance Lift PPos.SourcePos where
  lift p = [| $(liftData p) |]



deriving instance Lift AS_MapTo
deriving instance Lift AS_Field
deriving instance Lift AS_ExceptNav
deriving instance Lift AS_ExceptAssignment
deriving instance Lift AS_CaseArm
deriving instance Lift AS_RecordElementType
deriving instance Lift AS_QuantifierKind
deriving instance Lift AS_PrefixOp
deriving instance Lift AS_InfixOp
deriving instance Lift AS_PostfixOp
deriving instance Lift AS_Spec
deriving instance Lift AS_ExtendDecl
deriving instance Lift AS_QBoundN
deriving instance Lift AS_QBound1
deriving instance Lift AS_UnitDef
deriving instance Lift AS_OperatorHead
deriving instance Lift AS_Expression

instance Lift (Map VA_Value VA_Value) where
  lift p = [| $(liftData p) |]
instance Lift (Set VA_Value) where
  lift p = [| $(liftData p) |]
deriving instance Lift VA_Value


-- expr
tlaExpr :: String -> Q Exp
tlaExpr str = do
  case parseTLAExpr str of
    Left err -> error (show err)
    Right e  ->
      dataToExpQ (const Nothing `extQ` antiExprExp) e

antiExprExp :: AS_Expression -> Maybe (Q Exp)
antiExprExp (AS_MetaVar _i v) = Just $ varE (mkName v)
antiExprExp _                 = Nothing

tla_e :: QuasiQuoter
tla_e = QuasiQuoter tlaExpr err err err

-- nicer name for the most used qq
tla :: QuasiQuoter
tla = tla_e

parseTLAExpr :: String -> Either ParseError AS_Expression
parseTLAExpr s =
  runParser expression mkState "" s

tlaValue :: String -> Q Exp
tlaValue str = do
  case parseTLAExpr str of
    Left err  -> error (show err)
    Right expr ->
      case evalE [] expr of
        Left err ->
          error (ppError err)
        Right v ->
          [| v |]

tla_v :: QuasiQuoter
tla_v = QuasiQuoter tlaValue err err err


-- unit
tla_u :: QuasiQuoter
tla_u = QuasiQuoter tlaUnit err err err

parseTLAUnit :: String -> Either ParseError AS_UnitDef
parseTLAUnit s =
  runParser unit1 mkState "" s

tlaUnit :: String -> Q Exp
tlaUnit str = do
  case parseTLAUnit str of
    Left err -> error (show err)
    Right e  ->
      dataToExpQ (const Nothing `extQ` antiExprExp) e


-- spec
tla_s :: QuasiQuoter
tla_s = QuasiQuoter tlaSpec err err err

parseTLASpec :: String -> Either ParseError AS_Spec
parseTLASpec s =
  runParser tlaspec mkState "" s

tlaSpec :: String -> Q Exp
tlaSpec str = do
  case parseTLASpec str of
    Left err -> error (show err)
    Right e  ->
      dataToExpQ (const Nothing `extQ` antiExprExp) e


-- helpers
err = error "Only defined for values 2"
