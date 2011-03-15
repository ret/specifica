module GenCFG(genCFGFile, crashStartControlConst) where

import System (getArgs, exitFailure)
import Flatten
import Data.Generics
import Data.List( (\\) )
import Syntax
import RewriteTimer(allRoles)
import TLACodeGen(typeKernel, subst, Pattern, xify, allSingleMsgHandlerNames)
import Text.ParserCombinators.Parsec
import Language.TLAPlus.Parser( cfgspec, mkState )
import Language.TLAPlus.Syntax
import Language.TLAPlus.Pretty(prettyPrintCFG )
import Text.ParserCombinators.Parsec.Pos as PPos

boilerplate :: String -> SH_FL_Spec -> String
boilerplate pname spec = 
    unlines $ 
      ["CONSTANT"] ++ 
      (map (\s -> s ++ " <- " ++ xify s) $ allSingleMsgHandlerNames spec)

genCFGFile :: String -> SH_FL_Spec -> IO ()
genCFGFile pname spec =
    do { let configname = pname ++ ".config"
       ; c <- readConfig configname
       ; let b = boilerplate pname spec
       ; let roles = allRoles spec \\ ["GLOBAL"]
       ; let c' = complementConfig roles c
       ; let header = "\\* Generated file, please edit .config file"
       ; writeFile (pname ++ ".cfg") 
	   (unlines $ [header, prettyPrintCFG c',b])
       ; return ()
       }

readConfig :: String -> IO CFG_Config
readConfig cfgname =
    do { cfg <- readFile $ cfgname
       ; case (runParser cfgspec mkState cfgname cfg) of
           Left err -> do{ putStr $ "parse error in " ++ cfgname ++ " at "
			 ; print err
			 ; exitFailure
			 }
           Right cfg  -> return cfg
       }

complementConfig :: [String] -> CFG_Config -> CFG_Config
complementConfig roles config@(CFG_Config name l) =
    let cs = concat $ map (f config) roles
        cs' = if cs == [] then [] else [CFG_ConstantDef upos cs]
     in CFG_Config name $ l ++ cs'
  where f :: CFG_Config -> String -> [CFG_ConstantEntry]
	f config role = concat $ map (\name -> mk name config role)
			             crashStartControlConst 
	mk name config role = 
	    if has name role config 
	    then []
	    else [CFG_Assignment upos
		    (CFG_Ident upos $ name ++ role)
		    (CFG_Set upos empty)]
        has :: String -> String -> CFG_Config -> Bool
        has name role config =
	    [] /= (everything (++) ([] `mkQ` (g name role)) config)
	  where g name role a@(CFG_Assignment _ (CFG_Ident _ c) _)
	          | c == name ++ role = [True] -- e.g. "CrashR"
	          | otherwise = []
	        g _ _ _ = []

crashStartControlConst = ["InitDown", "Crash", "Start"]

---- HELPER -------------------------------------------------------------------
mk_AS_Ident s = AS_Ident epos [] s

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos name line col = newPos name line col

upos = mkPos "foo" 0 0 
epos = (upos, Nothing, Nothing)
