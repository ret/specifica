{
module Lexer (Token(..), AlexPosn(..), alexScanTokens, token_posn) where
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+				;
  $digit+				{ tok (\p s -> Int p (read s)) }
  $alpha [$alpha $digit \_]*		{ tok (\p s -> Atom p s) }
  \" (~\")* \"	                        { tok (\p s -> Str p (dropQuotes s)) }
  \=                                    { tok (\p s -> EQUAL p) }
  \[                                    { tok (\p s -> LAB p) }
  \]                                    { tok (\p s -> RAB p) }
  \{                                    { tok (\p s -> LCB p) }
  \}                                    { tok (\p s -> RCB p) }
  \(                                    { tok (\p s -> LB p) }
  \)                                    { tok (\p s -> RB p) }
  \,                                    { tok (\p s -> COMMA p) }
  \|\-\>                                { tok (\p s -> BARARROW p) }
  \<\<                                  { tok (\p s -> LTLT p) }
  \>\>                                  { tok (\p s -> GTGT p) }
  \:\>                                  { tok (\p s -> COLONGT p) }
  \@\@                                  { tok (\p s -> ATAT p) }
  \/\\                                  { tok (\p s -> SLBSL p) }

{
dropQuotes s = take (length s - 2) (tail s)

-- Each right-hand side has type :: AlexPosn -> String -> Token

-- Some action helpers:
tok f p s = f p s

-- The token type:
data Token =
    Int AlexPosn Int |
    Atom AlexPosn String	|
    Str AlexPosn String	|
    EQUAL AlexPosn |
    LAB AlexPosn |
    RAB AlexPosn |
    LCB AlexPosn |
    RCB AlexPosn |
    LB AlexPosn |
    RB AlexPosn |
    COMMA AlexPosn |
    BARARROW AlexPosn |
    LTLT AlexPosn |
    GTGT AlexPosn |
    COLONGT AlexPosn |
    ATAT AlexPosn |
    SLBSL AlexPosn
    deriving (Eq,Show)

token_posn (Int p _) = p
token_posn (Atom p _) = p
token_posn (Str p _) = p
token_posn (EQUAL p) = p
token_posn (LAB p) = p
token_posn (RAB p) = p
token_posn (LCB p) = p
token_posn (RCB p) = p
token_posn (LB p) = p
token_posn (RB p) = p
token_posn (COMMA p) = p
token_posn (BARARROW p) = p
token_posn (LTLT p) = p
token_posn (GTGT p) = p
token_posn (COLONGT p) = p
token_posn (ATAT p) = p
token_posn (SLBSL p) = p
}
