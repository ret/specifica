-- An example demonstrating how to connect a Happy parser to an Alex lexer.
{
module Parser (Stmt(..), Ident(..), Expr(..),
	       parser, listparser, statesectionparser, exprparser) where

-- avoid Map.! and Data.Array.! clash (happy, templates/GenericTemplate)
-- by hiding Map.!. Patch to happy sent to Simon Marlow for happy to qualify
-- use of ! for array access.
import Data.Map as Map hiding ((!), map)
import Data.Set as Set hiding (map)
import Lexer
}

%name parser Bind
%name listparser BindList
%name statesectionparser BindAnd
%name exprparser Expr
%tokentype { Token }

%token  int		{ Int _ $$  }
	atom		{ Atom _ $$  }
	str		{ Str _ $$  }
	'='		{ EQUAL _ }
	'['		{ LAB _ }
	']'		{ RAB _ }
	'{'		{ LCB _ }
	'}'		{ RCB _ }
	'('		{ LB _ }
	')'		{ RB _ }
	','		{ COMMA _ }
        '<<'            { LTLT _ }
        '>>'            { GTGT _ }
	'|->'	        { BARARROW _ }
	':>'	        { COLONGT _ }
	'@@'	        { ATAT _ }
	'/\\'	        { SLBSL _ }

%%

BindAnd :: { [Stmt] }
BindAnd : '/\\' Bind { [$2] }
        | BindAnd '/\\' Bind { $3:$1 }

BindList :: { [Stmt] }
BindList : Bind { [$1] }
         | BindList Bind { $2:$1 }

Bind :: { Stmt }
Bind  : Ident '=' Expr { Bind $1 $3 }

Ident :: { Ident }
Ident : atom { Ident $1 }

Expr :: { Expr }
Expr : '{'  SetE '}'   { SetE (Set.fromList $2) }
     | '<<' SeqE '>>'  { SeqE (reverse $2) }
     | '['  RecE ']'   { RecE (Map.fromList $2) }
     | '('  MapE ')'   { MapE (Map.fromList $2) }
     | int             { IntE $1 }
     | str             { StrE $1 }
     | atom            { AtomE $1 }

SetE :: { [Expr] }
SetE : {- empty -}   { [] }
     | Expr          { [$1] }
     | SetE ',' Expr { $3:$1 }

SeqE :: { [Expr] }
SeqE : {- empty -}   { [] }
     | Expr          { [$1] }
     | SeqE ',' Expr { $3:$1 }

RecE :: { [(Ident, Expr)] }
RecE : {- empty -}               { [] }
     | Ident '|->' Expr          { [($1, $3)] }
     | RecE ',' Ident '|->' Expr { ($3,$5):$1 }

MapE :: { [(Expr, Expr)] }
MapE : {- empty -}              { [] }
     | Expr ':>' Expr           { [($1, $3)] }
     | MapE '@@' Expr ':>' Expr { ($3,$5):$1 }


{
data Stmt = Bind Ident Expr deriving (Eq,Ord,Show)

data Ident = Ident String deriving (Eq,Ord,Show)

data Expr = RecE (Map Ident Expr) |
            MapE (Map Expr Expr) |
            SetE (Set Expr) |
            SeqE [Expr] |
            IntE Int |
            StrE String |
            AtomE String deriving (Eq,Ord,Show)

happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n")
	where
	lcn = 	case tks of
		  [] -> "end of file"
		  tk:_ -> "line " ++ show l ++ ", column " ++ show c
			where
			AlexPn _ l c = token_posn tk
}
