module ParserHelper where

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language( emptyDef )
import Text.ParserCombinators.Parsec.Pos as PPos
import Language.TLAPlus.Parser( mkState, operatorDef, expression )
import Language.TLAPlus.Syntax

inlineOperatorDef :: String -> AS_UnitDef
inlineOperatorDef s =
    case (runParser operatorDef mkState "" s) of
          Right ast -> ast
          Left err -> AS_OperatorDef upos
                         (AS_OpHead (mk_AS_Ident "ERROR") [])
                         (mk_AS_Ident $ show err)

inlineExpr :: String -> AS_Expression
inlineExpr s =
    case (runParser expression mkState "" s) of
          Right ast -> ast
          Left err -> (mk_AS_Ident $ show err)

mk_AS_Ident s = AS_Ident epos [] s

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos name line col = newPos name line col

upos = mkPos "foo" 0 0
epos = (upos, Nothing, Nothing)
