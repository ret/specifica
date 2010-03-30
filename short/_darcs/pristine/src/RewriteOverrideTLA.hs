module RewriteOverrideTLA(rewriteOverrideTLA) where

import Data.Generics
import Syntax
import Flatten
import Debug.Trace(trace)
import RewriteLifecycle(appendIL)
import Text.ParserCombinators.Parsec.Pos as PPos
import Language.TLAPlus.Syntax as TLASyntax

rewriteOverrideTLA :: SH_FL_Spec -> SH_FL_Spec
rewriteOverrideTLA = dropOverrideTLA . substTLA

dummyI = "dummyInteraction"

substTLA :: SH_FL_Spec -> SH_FL_Spec
substTLA spec = fixP (everywhere (mkT f)) spec
    where f t@(SH_VerbTLAOp _ homeInteraction Nothing tla) =
	      let (AS_OperatorDef _ 
		    (AS_OpHead (AS_Ident _ _ defname) args)
		    _oldexpr) = tla
		  override = findOverride spec (homeInteraction, defname)
	       in if override == []
	          then t
		  else let (i,oexpr) = head override -- head safe
		        in SH_VerbTLAOp upos i Nothing
		             (AS_OperatorDef upos 
			       (AS_OpHead (mk_AS_Ident defname) args)
			       oexpr)
	  f x = x
	  fixP f x = if f x == x then x else fixP f (f x)
 
findOverride :: SH_FL_Spec -> (String,String) -> [(String, AS_Expression)]
findOverride spec (i, defname) = (everything (++) ([] `mkQ` (f defname))) spec
    where f defname (SH_VerbTLAOp _ homeInteraction (Just oI) tla) 
	      | i `elem` oI = let (AS_OperatorDef _ 
			             (AS_OpHead (AS_Ident _ _ odefname) _args)
			             newexpr) = tla
			       in if defname == odefname
			          then [(homeInteraction, newexpr)]
			          else []
	      | otherwise = []
	  f _ _ = []

dropOverrideTLA :: SH_FL_Spec -> SH_FL_Spec
dropOverrideTLA spec = 
    let tla' = dropOverrideTLA0 (verbTLA spec)
     in spec { verbTLA = tla' }

dropOverrideTLA0 :: [SH_InteractionElement] ->  [SH_InteractionElement]
dropOverrideTLA0 = filter (not . isOverrideTLA)
  where isOverrideTLA (SH_VerbTLAOp _ _ (Just _) _) = True
	isOverrideTLA _ = False

---- HELPER -------------------------------------------------------------------
mk_AS_Ident s = AS_Ident epos [] s

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos name line col = newPos name line col

upos = mkPos "foo" 0 0 
epos = (upos, Nothing, Nothing)
