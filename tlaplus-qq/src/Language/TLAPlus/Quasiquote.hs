-- See:
--   http://dev.stephendiehl.com/hask/#quasiquotation
--   https://markkarpov.com/tutorial/th.html#example-2-creating-refined-values-at-compile-time

{-# LANGUAGE DeriveLift         #-}
{-# LANGUAGE QuasiQuotes        #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

module Language.TLAPlus.Quasiquote where

import           Language.Haskell.TH
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

import           Language.TLAPlus.Parser           (expression, mkState)
import           Language.TLAPlus.Pretty           (prettyPrintE, prettyPrintVA)
import           Language.TLAPlus.Syntax
import           Text.ParserCombinators.Parsec     (ParseError, runParser)
import           Text.ParserCombinators.Parsec.Pos as PPos

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

tlaExpr :: String -> Q Exp
tlaExpr str = do
  case parseTLAExpr str of
    Left err  -> error (show err)
    Right tag -> [| tag |]

tla :: QuasiQuoter
tla = QuasiQuoter tlaExpr err err err
  where err = error "Only defined for values 2"

parseTLAExpr :: String -> Either ParseError AS_Expression
parseTLAExpr s =
  runParser expression mkState "" s
