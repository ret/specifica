module RewriteStateInit(rewriteStateInit) where

import Data.Generics
import Syntax
import Flatten
import Text.ParserCombinators.Parsec.Pos as PPos
import Language.TLAPlus.Syntax as TLASyntax
import RewriteCont(typeDefaultValue)

-- this makes things like Map<.,.> very convenient since I don't have to
-- write up the awkward [x \in X |-> default-value(X)] 
rewriteStateInit :: SH_FL_Spec -> SH_FL_Spec
rewriteStateInit = everywhere (mkT f)
    where f (SH_State p per v@(ty, var) Nothing) = 
               {- Nothing = not initialized -}
	       SH_State p per v (Just (SH_ExprWrapper upos 
				         (typeDefaultValue ty)))
	  f x = x

---- HELPER -------------------------------------------------------------------
mk_AS_Ident s = AS_Ident epos [] s

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos name line col = newPos name line col

upos = mkPos "foo" 0 0 
epos = (upos, Nothing, Nothing)

