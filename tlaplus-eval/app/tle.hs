module Main where

import Language.TLAPlus.Eval( evalE, ppError )
import Language.TLAPlus.Parser( mkState, expression )
import Language.TLAPlus.Pretty (prettyPrintE, prettyPrintVA)
import Language.TLAPlus.Syntax
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Pos as PPos

main = do
  s <- getContents
  let exprAst = inlineExpr s
  case exprAst of
    Left err -> do
      putStrLn "** PARSE ERROR **"
      putStrLn $ show err
    Right e -> do
      putStrLn "** INPUT (pretty-printed):"
      putStrLn $ prettyPrintE e
      case evalE [] e of
        Left err -> do
          putStrLn "** EVALUATION ERROR **"
          putStrLn $ ppError err
        Right va -> do
          putStrLn ""
          putStrLn "** RESULT:"
          putStrLn $ prettyPrintVA va
  where
    inlineExpr :: String -> Either ParseError AS_Expression
    inlineExpr s =
      runParser expression mkState "" s
