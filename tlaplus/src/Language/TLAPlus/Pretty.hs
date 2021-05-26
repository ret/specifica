module Language.TLAPlus.Pretty
  ( prettyPrintAS, ppE, prettyPrintE,
    ppUnit,
    prettyPrintVA, ppVA, prettyPrintVATeX,
    prettyPrintCFG, ppCFG_Value) where

import           Data.List               (elemIndex, find)
import           Data.Map                as Map (foldrWithKey)
import           Data.Set                as Set (elems)
import           Prelude                 hiding ((<$>))
import           Text.PrettyPrint.Leijen

import           Language.TLAPlus.Syntax

prettyPrintAS :: AS_Spec -> String
prettyPrintAS spec =
  let extends = let AS_ExtendDecl _p l = extendDecl spec in map text l
   in showWidth 79 $
      text "----" <+> text "MODULE" <+> text (name spec) <+> text "----"
  <$> (if length extends > 0
       then text "EXTENDS" <+> align (fillCat $ punctuate comma extends)
       else empty)
  <$> ppUnitList (unitDef spec)
  <$> text "===="
  where showWidth :: Int -> Doc -> String
        showWidth w doc = displayS (renderPretty 0.9 w doc) ""

prettyPrintE :: AS_Expression -> String
prettyPrintE e =
  showWidth 79 $ ppE e
  where showWidth :: Int -> Doc -> String
        showWidth w doc = displayS (renderPretty 0.9 w doc) ""

ppUnitList :: [AS_UnitDef] -> Doc
ppUnitList l = vcat $ punctuate space (map ppUnit l)

ppUnit :: AS_UnitDef -> Doc
ppUnit (AS_FunctionDef _p headexpr qbounds expr) =
    ppFunctionDef headexpr qbounds expr
ppUnit (AS_OperatorDef _p (AS_OpHead h l) expr) =
    ppOperatorDef h l expr
ppUnit (AS_Assume _p e) = text "ASSUME" </> ppE e
ppUnit (AS_Theorem _p e) = text "THEOREM" </> ppE e
ppUnit (AS_ConstantDecl _p l) =
    text "CONSTANT" </> align(fillCat $ punctuate comma (map ppE l))
ppUnit (AS_RecursiveDecl _p l) =
    text "RECURSIVE" </> align(fillCat $ punctuate comma (map ppOperatorHead l))
ppUnit (AS_VariableDecl _p l) =
    text "VARIABLE" </> align(fillCat $ punctuate comma (map ppE l))
ppUnit (AS_Separator _p) = text "----"


ppE :: AS_Expression -> Doc
ppE (AS_Ident _info quallist name) =
    let l = map text $ quallist++[name] in cat $ punctuate (text "!") l
ppE (AS_FunArgList _info l) = (cat $ punctuate comma (map ppE l)) <//> text "]"
ppE (AS_OpApp _info e l) =
    let args = if l == []
               then empty
               else parens( align( cat $ punctuate comma (map (group . ppE) l)))
     in group (ppE e <//> group(args))
ppE (AS_FunctionType _info a b) = brackets $ ppE a </> text "->" </> ppE b
-- hack to make sure that SUBSET (A \X B) is printed with the () in place,
-- otherwise TLC will complain about ambigious precedence. Note that \X isn't
-- really an infix operators (see book, p. 284)
ppE p@(AS_PrefixOP _info op e) = ppPrefixOP op <//> parens (protectE p e)
ppE p@(AS_PostfixOP _info op e) = protectE p e <//> ppPostfixOP op
ppE p@(AS_InfixOP _loc op a b) =
    if op `elem` tightop
      then group( protectE p a <//> ppInfixOP op <//> protectE p b )
      else group( protectE p a <+> ppInfixOP op <+> protectE p b )
    where tightop = [AS_DOT, AS_DOTDOT, AS_Plus, AS_Minus, AS_FunApp]
ppE (AS_Let _info l e) = align((text "LET" <+> (align (ppUnitList l)))
                               <$> group (text " IN" <+> align (ppE e)))
ppE (AS_IF _info cond a b) =
    align(    text "IF" <+> align (ppE cond)
              <$> text "THEN" <+> align (ppE a)
              <$> text "ELSE" <+> align (ppE b))
ppE (AS_DiscreteSet _info l) = braces $ cat (punctuate comma $ map ppE l)
ppE (AS_RecordFunction _info l) =
    let l' = map (\(AS_MapTo (AS_Field s) e) ->
                      group( text s <+> text "|->" <+> ppE e)) l
     in brackets $ align (cat (punctuate comma l'))
ppE (AS_QuantifierBoundFunction _info bounds e) =
    let b = cat (punctuate comma (map ppQBoundN bounds))
     in brackets $ align (b <+> text "|->" </> align (ppE e))
ppE (AS_Choose _info b e) =
    text "CHOOSE" <+> align (ppQBound1 b <//> text ":" <+> align (ppE e))
ppE (AS_Quantified _info kind bounds e) =
    let k = case kind of
              AS_All   -> text "\\A"
              AS_Exist -> text "\\E"
        b = cat (punctuate comma (map ppQBoundN bounds))
     in k <+> align (b <//> text ":" <+> ppE e)
ppE (AS_Tuple _info l) =
    group( text "<<" <//>
           align (cat (punctuate comma $ map (group . ppE) l)) <//>
           text ">>")
-- FIXME kramer@acm.org reto -- the only reason I need to wrap the
-- ppE in parens is so that a case like
--   Next == \E r: a(r) \/ \E r: b(r)
-- does not lead to a warning that r has already been used
-- the parens avoids this (relevant in case the formula is pretty printed
-- on one line).
-- The alternative (prefered) would be to always force the LOR and LAND onto
-- mutliple lines
ppE (AS_LAND _pos le) =
    let lt = map (\e -> text "/\\" <+> ppE e) le
     in align $ vsep lt
ppE (AS_LOR _pos le) =
    let lt = map (\e -> text "\\/" <+> ppE e) le
     in align $ vsep lt

ppE (AS_Num _info n) = integer n
ppE (AS_Bool _info b) = text $ if b then "TRUE" else "FALSE"
ppE (AS_StringLiteral _info s) = dquotes $ text s
ppE (AS_RecordType _info l) =
    let t = map (\(AS_RecordElementType _info (AS_Field s) e) ->
                    text s <//>text ":"<+> ppE e) l
     in brackets $ align (cat (punctuate comma t))
ppE (AS_SetComprehension _info b e) =
    braces $ align (ppQBound1 b <//> text ":" <+> align (ppE e))
ppE (AS_SetGeneration _info e bl) =
    braces $ align (ppE e <//> text ":" <+> align (ppQBoundN bl))
ppE (AS_Except e l) =
    let a = map ( \ (AS_ExceptAssignment navs e) ->
                     let ns = cat $ map (\n ->
                           case n of AS_ExceptNavField (AS_Field s) ->
                                       text "." <//> text s
                                     AS_ExceptNavApp es ->
                                       let es' = punctuate comma (map ppE es)
                                        in brackets $ cat es') navs
                      in group(     text "!"
                               <//> ns
                               <+>  text "="
                               <+>  align (ppE e)))
            l
        b = group $ vcat (punctuate comma a)
     in group( brackets (align( group( (group (ppE e)
                                 <+> text "EXCEPT" <+> group (align b))))))
ppE (AS_OldVal) = text "@"
ppE (AS_Stutter a b) = (brackets $ ppE a) <//> text "_" <//> ppE b
ppE (AS_Fair strong a b) =
         (if strong then text "SF" else text "WF")
    <//> text "_" <//> ppE b <//> (parens $ ppE a)
ppE (AS_Case _ arms other) = -- FIXME missing the pretty print of []
    let as  = map (\(AS_CaseArm _ a b) ->
                ppE a <+> text "->" <+> align (ppE b)) arms
        as' = case other of
                Nothing -> as
                Just (AS_OtherCaseArm _ e) ->
                    as ++ [text "OTHER" <+> text "->" <+> align (ppE e)]
                Just (AS_CaseArm _ _ _) -> error "unspecified"
     in text "CASE" <+> align (vcat $ punctuate (text " [] ") as')
ppE (AS_BIF mod name) = text "<builtin" <+>
                        text mod <//> text "!" <//>
                        text name <//> text ">"
ppE (AS_CloseFunApp) = text "]"
-- missing pretty printer support, debug aid
-- ppE e = text $ "(<?>" ++ (show e) ++ "<?>)"

ppFunctionDef headexpr qbounds expr =
    let bounds = group (cat $ punctuate comma $ map ppQBoundN qbounds)
     in     group( ppE headexpr <//> brackets (align bounds) <+> text "==")
        </> group (ppE expr)

ppOperatorDef h l expr =
    let args = if length l > 0
               then parens (cat (punctuate comma (map ppE l)))
               else empty
     in     group (ppE h <//> args <+> text "==")
        <$> indent 2 (align (group (ppE expr)))

ppOperatorHead (AS_OpHead h l) =
    let args = if length l > 0
               then parens (cat (punctuate comma (map ppE l)))
               else empty
     in group (ppE h <//> args)

ppQBoundN :: AS_QBoundN -> Doc
ppQBoundN (AS_QBoundN vars expr) =
    cat (punctuate comma (map ppE vars)) </> text "\\in" </> ppE expr

ppQBound1 :: AS_QBound1 -> Doc
ppQBound1 (AS_QBound1 var expr) =
    ppE var <+> text "\\in" </> ppE expr

ppPrefixOP :: AS_PrefixOp -> Doc
ppPrefixOP AS_SUBSET     = text "SUBSET" <//> space
ppPrefixOP AS_INSTANCE   = text "INSTANCE" <//> space
ppPrefixOP AS_UNION      = text "UNION" <//> space
ppPrefixOP AS_DOMAIN     = text "DOMAIN" <//> space
ppPrefixOP AS_UNCHANGED  = text "UNCHANGED" <//> space
ppPrefixOP AS_Not        = text "~"
ppPrefixOP AS_Neg        = text "-"
ppPrefixOP AS_ALWAYS     = text "[]"
ppPrefixOP AS_Eventually = text "<>"

ppPostfixOP :: AS_PostfixOp -> Doc
ppPostfixOP AS_Prime = text "'"

ppInfixOP :: AS_InfixOp -> Doc
ppInfixOP AS_EQ          = text "="
ppInfixOP AS_NEQ         = text "#"
ppInfixOP AS_COLONGT     = text ":>"
ppInfixOP AS_ATAT        = text "@@"
ppInfixOP AS_DOTDOT      = text ".."
ppInfixOP AS_DOT         = text "."
ppInfixOP AS_GT          = text ">"
ppInfixOP AS_LT          = text "<"
ppInfixOP AS_LTEQ        = text "<="
ppInfixOP AS_GTEQ        = text ">="
ppInfixOP AS_SubsetEq    = text "\\subseteq"
ppInfixOP AS_Cup         = text "\\cup"
ppInfixOP AS_Union       = text "\\union"
ppInfixOP AS_Cap         = text "\\cap"
ppInfixOP AS_Intersect   = text "\\intersect"
ppInfixOP AS_SetMinus    = text "\\"
ppInfixOP AS_In          = text "\\in"
ppInfixOP AS_Circ        = text "\\o"
ppInfixOP AS_NotIn       = text "\\notin"
ppInfixOP AS_Plus        = text "+"
ppInfixOP AS_DIV         = text "\\div"
ppInfixOP AS_MOD         = text "%"
ppInfixOP AS_Mult        = text "*"
ppInfixOP AS_Minus       = text "-"
ppInfixOP AS_Times       = text "\\X"
ppInfixOP AS_AND         = text "/\\"
ppInfixOP AS_OR          = text "\\/"
ppInfixOP AS_Implication = text "=>"
ppInfixOP AS_TildeGT     = text "~>"
ppInfixOP AS_FunApp      = text "[" -- close by postix (noop) AS_CloseFunApp

protectE :: AS_Expression -> AS_Expression -> Doc
protectE parent e = if prec e < prec parent
                    then group( parens $ ppE e )
                    else group( ppE e )

prec :: AS_Expression -> Int
prec (AS_PrefixOP _info op _e)  = prefixPrec op table
prec (AS_PostfixOP _info op _e) = postfixPrec op table
prec (AS_InfixOP _loc op _a _b) = let (p, _) = infixPrec op table in p
prec _                          = 999

-- _assoc :: AS_Expression -> Assoc
-- _assoc (AS_InfixOP _loc op _a _b) = let (_, a) = infixPrec op table in a

prefixPrec :: AS_PrefixOp -> PrecedenceTable -> Int
prefixPrec op t =
    let Just l = find (f op) t      -- operator must be in the table!
        Just i = elemIndex l t      -- must be in table
     in length t - i
    where f :: AS_PrefixOp -> [Fix] -> Bool
          f op l = Nothing /= find (\f -> case f of
                                      Prefix _name theop -> theop == op
                                      _                  -> False) l

postfixPrec :: AS_PostfixOp -> PrecedenceTable -> Int
postfixPrec op t =
    let Just l = find (f op) t      -- operator must be in the table!
        Just i = elemIndex l t      -- must be in table
     in length t - i
    where f :: AS_PostfixOp -> [Fix] -> Bool
          f op l = Nothing /= find (\f -> case f of
                                      Postfix _name theop -> theop == op
                                      _                   -> False) l

infixPrec :: AS_InfixOp -> PrecedenceTable -> (Int, Assoc)
infixPrec op t =
    let Just l = find (f op) t      -- operator must be in the table!
        Just i = elemIndex l t      -- must be in table
        Just (Infix _ _ assoc) =
            find (\(Infix _ operator _) -> operator == op) l
     in (length t - i, assoc)
    where f :: AS_InfixOp -> [Fix] -> Bool
          f op l = Nothing /= find (\f -> case f of
                                      Infix _name theop _a -> theop == op
                                      _                    -> False) l

{----------------------------------------------------------}
{- KEEP THIS TABLE IN SYNC WITH THE MASTER COPY IN PARSER -}
{----------------------------------------------------------}
type PrecedenceTable = [[Fix]]
table :: PrecedenceTable
table =
    [{-17/17-}[binary "."          (op_infix  AS_DOT)      AssocLeft]
    ,{-16/16-}[binaryS "["         (op_infixS AS_FunApp)   AssocLeft]
    ,{-15/15-}[postfix "'"         (op_postfix AS_Prime)]
     {-14/14-}
    ,{-13/13-}[binary "\\div"      (op_infix  AS_DIV)      AssocLeft
              ,binary "\\mod"      (op_infix  AS_MOD)      AssocLeft
              ,binary "%"          (op_infix  AS_MOD)      AssocLeft
              ,binary "*"          (op_infix  AS_Mult)     AssocLeft
              ,binary "\\X"        (op_infix  AS_Times)    AssocLeft -- ??
              ,binary "\\times"    (op_infix  AS_Times)    AssocLeft -- ??
              ,binary "\\o"        (op_infix  AS_Circ)     AssocLeft
              ,binary "\\circ"     (op_infix  AS_Circ)     AssocLeft]
    ,{-12/12-}[prefix "-"          (op_prefix AS_Neg)]               -- prio 12, book p. 271
    ,{-11/11-}[binary "-"          (op_infix  AS_Minus)    AssocLeft]
    ,{-10/10-}[binary "+"          (op_infix  AS_Plus)     AssocLeft]
    ,{- 9/ 9-}[binary ".."         (op_infix  AS_DOTDOT)   AssocNone
              ,prefix "DOMAIN"     (op_prefix AS_DOMAIN) ]
    ,{- 8/ 8-}[binary "\\cup"      (op_infix  AS_Cup)      AssocLeft
              ,binary "\\union"    (op_infix  AS_Union)    AssocLeft
              ,binary "\\cap"      (op_infix  AS_Cap)      AssocLeft
              ,binary "\\intersect"(op_infix  AS_Intersect)AssocLeft
              ,binary "\\"         (op_infix  AS_SetMinus) AssocNone
              ,prefix "SUBSET"     (op_prefix AS_SUBSET)
              ,prefix "UNION"      (op_prefix AS_UNION) ]
    ,{- 7/ 7-}[binary ":>"         (op_infix  AS_COLONGT)  AssocNone]
    ,{- 6/ 6-}[binary "@@"         (op_infix  AS_ATAT)     AssocLeft]
    ,{- 5/ 5-}[binary "\\subseteq" (op_infix  AS_SubsetEq) AssocNone
              ,binary "<="         (op_infix  AS_LTEQ)     AssocNone
              ,binary "<"          (op_infix  AS_LT)       AssocNone
              ,binary ">"          (op_infix  AS_GT)       AssocNone
              ,binary ">="         (op_infix  AS_GTEQ)     AssocNone
              ,binary "="          (op_infix  AS_EQ)       AssocNone
              ,binary "#"          (op_infix  AS_NEQ)      AssocNone
              ,binary "/="         (op_infix  AS_NEQ)      AssocNone
              ,binary "\\in"       (op_infix  AS_In)       AssocNone
              ,binary "\\notin"    (op_infix  AS_NotIn)    AssocNone]
    ,{- 4/ 4-}[prefix "~"          (op_prefix AS_Not)
              ,prefix "UNCHANGED"  (op_prefix AS_UNCHANGED)
              ,prefix "[]"         (op_prefix AS_ALWAYS)
              ,prefix "<>"         (op_prefix AS_Eventually)]
    -- we will not print () around the /\ in '(b /\ x) \/ y'
    ,{- 3/ 3-}[binary "/\\"        (op_infix  AS_AND)      AssocLeft]
    -- precendence lower than /\, will print (...) arond \/ in e.g. 'b /\ (x \/ y)'
    ,{-25/25-}[binary "\\/"        (op_infix  AS_OR)       AssocLeft]
    ,{- 2/ 2-}[binary "~>"         (op_infix  AS_TildeGT)  AssocNone]
    ,{- 1/ 1-}[binary "=>"         (op_infix  AS_Implication) AssocNone
              ,prefix "INSTANCE"   (op_prefix AS_INSTANCE) ] -- ?? operator
    ]

data Assoc = AssocLeft | AssocRight | AssocNone deriving (Eq, Ord, Show)
data Fix = Infix   String AS_InfixOp Assoc
         | Prefix  String AS_PrefixOp
         | Postfix String AS_PostfixOp deriving (Eq, Ord, Show)
op_prefix :: AS_PrefixOp -> AS_PrefixOp
op_prefix op = op
op_postfix :: AS_PostfixOp -> AS_PostfixOp
op_postfix op = op
-- op_postfixIgnore :: AS_PostfixOp -> AS_PostfixOp
-- op_postfixIgnore op = op -- do not ignore for display purposes
-- op_postfixS :: AS_InfixOp -> AS_InfixOp -- MUST be Infix, used for x[y]
-- op_postfixS op = op
op_infix :: AS_InfixOp -> AS_InfixOp
op_infix op = op
op_infixS :: AS_InfixOp -> AS_InfixOp
op_infixS op = op
binary  name op assoc = Infix   name op assoc
binaryS name op assoc = Infix   name op assoc
prefix  name op       = Prefix  name op
postfix name op       = Postfix name op
-- postfixS op           = Infix "" op AssocLeft -- MUST be Infix, used for x[y]

-------------------------------------------------------------------------------

prettyPrintVA :: VA_Value -> String
prettyPrintVA v =
   showWidth 79 $ ppVA v
  where showWidth :: Int -> Doc -> String
        showWidth w doc = displayS (renderPretty 0.9 w doc) ""

ppVA :: VA_Value -> Doc
ppVA (VA_Map m) =
    let l = Map.foldrWithKey
              (\k e acc -> (ppVA k <+> text "|->" <+> ppVA e):acc) [] m
     in brackets $ align (vcat (punctuate comma l))
ppVA (VA_Rec m) =
    let l = Map.foldrWithKey (\(VA_String k) e acc ->
              (text k <+> text "|->" <+> ppVA e):acc) [] m
     in brackets $ align (vcat (punctuate comma l))
ppVA (VA_Set s) =
    let l = Set.elems s in braces $ align (cat (punctuate comma $ map ppVA l))
ppVA (VA_Seq l) =
    group( text "<<" <//>
           align (cat (punctuate comma $ map (group . ppVA) l)) <//>
           text ">>")
ppVA (VA_Int i) = integer i
ppVA (VA_Bool b) = if b then text "TRUE" else text "FALSE"
ppVA (VA_String s) = dquotes $ text s
ppVA (VA_Char c) = char '\'' <//> char c <//> char '\''
ppVA (VA_Atom a) = text a
ppVA (VA_FunctionDef _info head quants expr) =
    ppFunctionDef head quants expr
ppVA (VA_OperatorDef _info (AS_OpHead head args) expr) =
    ppOperatorDef head args expr
ppVA (VA_FunType a b) =
    brackets $ align (ppVA a <+> text "->" <+> ppVA b)
ppVA (VA_RecType m) =
    let l = Map.foldrWithKey (\(VA_String k) v acc ->
              (text k <//> text ":" <+>
              ppVA v):acc) [] m
     in brackets $ align (vcat (punctuate comma l))
ppVA (VA_SeqType v) =
    text "Seq" <//> parens (ppVA v)
ppVA (VA_Var v) = case v of
                    Nothing -> text "unbound"
                    Just v' -> ppVA v'
-- FIXME remove the closing ], opening is printed by FunApp
ppVA (VA_FunArgList l) = brackets $ cat $ punctuate comma (map ppVA l)

prettyPrintVATeX :: VA_Value -> String
prettyPrintVATeX v =
   showWidth 79 $ ppVATeX v
  where showWidth :: Int -> Doc -> String
        showWidth w doc = displayS (renderPretty 0.9 w doc) ""

ppVATeX :: VA_Value -> Doc
ppVATeX (VA_Map m) =
    let l = Map.foldrWithKey
              (\k e acc -> (ppVATeX k <+> text "$\\mapsto$" <+>
                            ppVATeX e):acc) [] m
     in brackets $ align (hcat (punctuate (comma <//> space) l))
ppVATeX (VA_Rec m) =
    let l = Map.foldrWithKey (\(VA_String k) e acc ->
              (text (teX k) <+> text "$\\mapsto$" <+> ppVATeX e):acc) [] m
     in brackets $ align (hcat (punctuate (comma <//> space) l))
ppVATeX (VA_Set s) =
    let l = Set.elems s
     in text "\\{" <//>
        align (hcat (punctuate (comma <//> space) $ map ppVATeX l)) <//>
        text "\\}"
ppVATeX (VA_Seq l) =
    text "$\\ll$" <//>
    align (hcat (punctuate (comma <//> space) $ map ppVATeX l)) <//>
    text "$\\gg$"
ppVATeX (VA_Int i) = integer i
ppVATeX (VA_Bool b) = if b then text "TRUE" else text "FALSE"
ppVATeX (VA_String s) = text "{\\tt \"" <//> text (teX s) <//> text "\"}"
ppVATeX (VA_Char c) = char '\'' <//> char c <//> char '\''
ppVATeX (VA_Atom a) = text (teX a)
ppVATeX (VA_FunctionDef _info head quants expr) =
    ppFunctionDef head quants expr
ppVATeX (VA_OperatorDef _info (AS_OpHead head args) expr) =
    ppOperatorDef head args expr
ppVATeX (VA_FunType a b) =
    brackets $ align (ppVATeX a <+> text "->" <+> ppVATeX b)
ppVATeX (VA_RecType m) =
    let l = Map.foldrWithKey (\(VA_String k) v acc ->
              (text (teX k) <//> text ":" <+>
              ppVATeX v):acc) [] m
     in brackets $ align (hcat (punctuate (comma <//> space) l))
ppVATeX (VA_SeqType v) =
    text "Seq" <//> parens (ppVATeX v)
ppVATeX (VA_Var v) = case v of
                    Nothing -> text "unbound"
                    Just v' -> ppVATeX v'
ppVATeX (VA_FunArgList l) = brackets $ cat $ punctuate comma (map ppVATeX l)

teX :: String -> String
teX  = map (\c -> case c of
                    '_' -> '-'
                    _   -> c)

-------------------------------------------------------------------------------

prettyPrintCFG :: CFG_Config -> String
prettyPrintCFG v =
   showWidth 79 $ ppCFG_Config v
  where showWidth :: Int -> Doc -> String
        showWidth w doc = displayS (renderPretty 0.9 w doc) ""

ppCFG_Config :: CFG_Config -> Doc
ppCFG_Config (CFG_Config (Just n) l) = text "SPECIFICATION" <+> text n <$>
                                       vcat (map ppCFG_Statement l)
ppCFG_Config (CFG_Config Nothing l)  = vcat (map ppCFG_Statement l)

ppCFG_Statement :: CFG_Statement -> Doc
ppCFG_Statement (CFG_ConstantDef _info l) =
    nest 4 (text "CONSTANT" <$> vcat (map ppCFG_ConstantEntry l))
ppCFG_Statement (CFG_Invariant _info l) =
    nest 4 (text "INVARIANT" <$> vcat (map ppCFG_Ident l))
ppCFG_Statement (CFG_Property _info l) =
    nest 4 (text "PROPERTY" <$> vcat (map ppCFG_Ident l))
ppCFG_Statement (CFG_Symmetry _info i) =
    text "SYMMETRY" <+> ppCFG_Ident i
ppCFG_Statement (CFG_View _info i) =
    text "VIEW" <+> ppCFG_Ident i

ppCFG_ConstantEntry :: CFG_ConstantEntry -> Doc
ppCFG_ConstantEntry (CFG_Assignment _info i v) =
    ppCFG_Ident i <+> text "=" <+> align (ppCFG_Value v)
ppCFG_ConstantEntry (CFG_Subst _info a b) =
    ppCFG_Ident a <+> text "<-" <+> ppCFG_Ident b

ppCFG_Ident :: CFG_Ident -> Doc
ppCFG_Ident (CFG_Ident _info s) = text s

ppCFG_Value :: CFG_Value -> Doc
ppCFG_Value (CFG_Atom _info s) = text s
ppCFG_Value (CFG_Bool _info b) = if b then text "TRUE" else text "FALSE"
ppCFG_Value (CFG_Int _info i) = integer i
ppCFG_Value (CFG_StringLiteral _info s) = dquotes $ text s
ppCFG_Value (CFG_Set _info s) =
    let l = Set.elems s
     in braces $ align (cat (punctuate comma $ map ppCFG_Value l))
