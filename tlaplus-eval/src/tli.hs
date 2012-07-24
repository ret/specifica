module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, stdout)
import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Text.ParserCombinators.Parsec.Language( emptyDef )

import Language.TLAPlus.Parser( tlaspec, cfgspec, mkState )
import Language.TLAPlus.Syntax
import Language.TLAPlus.Pretty( prettyPrintAS, prettyPrintVA, 
                                prettyPrintCFG )
import Language.TLAPlus.Eval

main :: IO ()
main = 
    do{ args <- getArgs
      ; let fname = args !! 0
      ; let tlaname = case reverse fname of
                      ('a':'l':'t':'.':fn) -> fname
                      otherwise -> fname ++ ".tla"
      ; let cfgname = case reverse fname of
                      ('a':'l':'t':'.':fn) -> reverse fn ++ ".cfg"
                      otherwise -> fname ++ ".cfg"
      ; cfg <- readFile $ cfgname
      ; case (runParser cfgspec mkState cfgname cfg) of
          Left err -> do{ putStr "cfg configuration parse error at "
                        ; print err
                        ; exitFailure
                        }
          Right cfg  -> 
            do{ res <- readTLA tlaname
              ; case res of 
                  Left err -> do{ putStr "tla specification parse error at "
                                ; print err 
                                }
                  Right l  ->
                    do{ mapM_ (\tla -> 
                               do{ putStrLn $ prettyPrintCFG cfg
                                 ; putStrLn $ prettyPrintAS tla
                                 ; putStrLn $ show tla
                                 }) l
                      ; let res = eval (reverse l) cfg
                      ; let ls = case res of
                                     Left err -> [ppError err]
                                     Right res' -> map prettyPrintVA res'
                      ; putStrLn $ unlines ls
                      ; return ()
                      }
            }
      }

readTLA :: String -> IO (Either ParseError [AS_Spec])
readTLA fname = 
    do{ let tlaname = case reverse fname of
                        ('a':'l':'t':'.':fn) -> fname
                        otherwise -> fname ++ ".tla"
      ; tla <- readFile $ tlaname
      ; case runParser tlaspec mkState tlaname tla of
          Left err -> return $ Left $ err
          Right tlaspec  ->
            let (AS_ExtendDecl _info l) = extendDecl tlaspec
             in mapM readTLA l >>= \as -> 
               let as' = filter (\t -> case t of 
                                         Left err -> False 
                                         Right t -> True) as 
                in return $ 
                     Right ([tlaspec] ++ (concat $ map (\(Right t) -> t) as'))
      }
