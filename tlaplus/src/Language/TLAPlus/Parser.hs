{------------------------------------------------------------------------------
ISSUES
  - ISSUE: a[1].b.c.d not supported (a[1].b.c is), i.e. only 2 dots are
    supported following a function application (or record reference).
    WORKAROUND 1: LET x = a[1].b.c IN x.d
    WORKAROUND 2: (a[1].b.c).d

  - ISSUE: f[g[h]] not supported, g is unexpected.
    WORKAROUND: f[ (g[h]) ], add parenthesis around inner g[h]
------------------------------------------------------------------------------}

module Language.TLAPlus.Parser
    (tlaspec, cfgspec, table, mkState,
     expression, operatorDef) -- for use in australis
where

import           Data.Char                              (isAlpha)
import           Data.List                              (nub, (\\))
import           Data.Set                               as Set (fromList)
import           Debug.Trace                            as Trace
import           Text.ParserCombinators.Parsec
import           Text.ParserCombinators.Parsec.Expr
import           Text.ParserCombinators.Parsec.Language (emptyDef)
import           Text.ParserCombinators.Parsec.Pos      (SourcePos)
import qualified Text.ParserCombinators.Parsec.Token    as P

import           Language.TLAPlus.ParserState           (PState, TLAParser,
                                                         mkState, popIndent,
                                                         pushIndent)
import           Language.TLAPlus.Pretty                (prettyPrintE)
import           Language.TLAPlus.Syntax

tlaspec :: TLAParser AS_Spec
tlaspec = do { whiteSpace
             ; m <- headerSpec <?> "header"
             ; e <- extends
             ; u <- unit
             ; return AS_Spec{ name = m
                             , extendDecl = e
                             , unitDef = u }
             }

dashSep :: TLAParser AS_UnitDef
dashSep = do{ p <- getPosition
            ; skipMany1 $ ((char '-') <?> "separator (----)")
            ; whiteSpace
            ; return $ AS_Separator p
            }

specEnd :: TLAParser ()
specEnd = do{ _ <- count 4 (char '=' <?> "specification end (====)")
            ; whiteSpace
            }

headerSpec :: TLAParser String
headerSpec = do{ _ <- dashSep
               ; reserved "MODULE"
               ; m <- identifier <?> "module name"
               ; _ <- dashSep
               ;  return m
               }

extends :: TLAParser AS_ExtendDecl
extends = do{ p <- getPosition
            ; option (AS_ExtendDecl p []) $
              do{
                ; reserved "EXTENDS"
                ; l <- commaSep identifier
                ; return $ AS_ExtendDecl p l
                }
            }

constant :: TLAParser AS_UnitDef
constant = do{ p <- getPosition
             ; reserved "CONSTANT" <|> reserved "CONSTANTS"
             ; l <- commaSep qualident -- too liberal, don't allow qual
             ; return $ AS_ConstantDecl p l
             }

-- p.2, of https://lamport.azurewebsites.net/tla/tla2-guide.pdf
recursive :: TLAParser AS_UnitDef
recursive = do{ p <- getPosition
             ; reserved "RECURSIVE"
             ; l <- commaSep operatorHead
             ; return $ AS_RecursiveDecl p l
             }

variable :: TLAParser AS_UnitDef
variable = do{ p <- getPosition
             ; reserved "VARIABLE" <|> reserved "VARIABLES"
             ; l <- commaSep qualident -- too liberal, don't allow qual
             ; return $ AS_VariableDecl p l
             }

unit :: TLAParser [AS_UnitDef]
unit = do{ l <- sepBy (choice [ -- try as cheap way to left factor identifier
                         try operatorDef
                       , try funDef
                       , constant
                       , recursive
                       , variable
                       , assume
                       , theorem
                       , dashSep
                       ])
                  whiteSpace
         ; specEnd
         ; return l
         }

mkIdent :: SourcePos -> [String] -> String -> AS_Expression
mkIdent p qual name = AS_Ident (mkInfo p) qual name

mkInfo :: SourcePos -> AS_InfoE
mkInfo p = (p, Nothing, Nothing)

qualident :: TLAParser AS_Expression
qualident = do{ p <- getPosition
--              ; l <- sepBy1 identifier (char '!') -- for TLA+
-- FIXME, for australis/short: meaning of ! is incompatible with TLA+, ifdef?
              ; i <- identifier -- HACK FIXME, Short does use the ! for
              ; let l = [i]     -- message sending!
              ; let lr = reverse l
                 in return $ mkIdent p (reverse $ tail lr) (head lr)
              }

operatorHead :: TLAParser AS_OperatorHead
operatorHead = do{ qname <- qualident
                 ; args <- option [] (parens $ commaSep operatorHeadEntry)
                 ; return $ AS_OpHead qname args
                 }

operatorHeadEntry :: TLAParser AS_Expression
operatorHeadEntry = do{ p <- getPosition
                      ; qname <- qualident
                      ; l <- option [] (parens $ commaSep expression)
                      ; if l == []
                        then return qname
                        else return $ AS_OpApp (mkInfo p) qname l
                      }

operatorDef :: TLAParser AS_UnitDef
operatorDef = do{ p <- getPosition
                ; local <- option False (do reserved "LOCAL"; return True)
                ; h <- operatorHead
                ; reservedOp "=="
                ; e <- expression
                ; return $ AS_OperatorDef p local h e
                }

funDef :: TLAParser AS_UnitDef
funDef = do{ p <- getPosition
           ; local <- option False (do reserved "LOCAL" ; return True)
           ; qname <- qualident
           ; args <- squares $ commaSep quantifierBound
           ; reservedOp "=="
           ; expr <- expression
           ; return $ AS_FunctionDef p local qname args expr
           }

assume :: TLAParser AS_UnitDef
assume = do{ p <- getPosition
           ; reserved "ASSUME"
           ; e <- expression
           ; return $ AS_Assume p e
           }

theorem :: TLAParser AS_UnitDef
theorem = do{ p <- getPosition
            ; reserved "THEOREM"
            ; e <- expression
            ; return $ AS_Theorem p e
            }

quantifierBound :: TLAParser AS_QBoundN
quantifierBound = do{ idsOrTuple <-     (do t <- gtgtExpr    -- AS_Tuple
                                            return [t])
                                    <|> (commaSep qualident) -- AS_Ident
                    ; reservedOp "\\in"
                    ; expr <- expression
                    ; return $ AS_QBoundN idsOrTuple expr
                    }

quantifierBound1 :: TLAParser AS_QBound1
quantifierBound1 = do{ id <- qualident
                     ; reservedOp "\\in"
                     ; expr <- expression
                     ; return $ AS_QBound1 id expr
                     }

expression :: TLAParser AS_Expression
expression = do
  aoExpr

-- Expression with binary /\ and \/
aoExpr :: TLAParser AS_Expression
aoExpr = do
  p <- getPosition
  (e, mL) <- sepBy1NotLeft (sourceColumn p) (do try nonAOExpression)
  case mL of
    [] -> -- no \/ or /\ present
      --return $ Trace.trace ("@E " ++ show e) e
      return e
    [(op,f)] -> do -- binary case X /\ Y, or X \/ Y
      let x = AS_InfixOP (mkInfo p) op e f
      --return $ Trace.trace ("@X " ++ show x) x
      return x
    _ -> do
      -- 3 or more expressions with (potentially) a mix of \/ and /\
      if (sameOp AS_AND (map fst mL)) || (sameOp AS_OR (map fst mL))
        then do
          -- use of head is safe here because of outer 'case mL...' check
          let res = buildInfixTree e (fst $ head mL) (map snd mL)
          --return $ Trace.trace ("### X = "++ show res) res
          return res
        else do
          -- We are a little more agressive complaining here than
          -- strictly needed, if we gave /\ precendence over \/.
          -- Our error message matches TLC's (empiracally tested):
          --   TLC input: b \/ x /\ y
          --     Error evaluating expression:
          --     Precedence conflict between ops \lor in block line 4, col 3 to line 4, col 4 of module ... and \land.
          --     Parsing or semantic analysis failed.
          let estr = concatMap (\(op,e) -> "\n  " ++ show op ++ " " ++ prettyPrintE e ++ " (at " ++ ppLocE e ++ ")") mL
          fail ("Precedence conflict between: "++ estr)
  where
    sameOp :: AS_InfixOp -> [AS_InfixOp] -> Bool
    sameOp op = all (== op)
    buildInfixTree :: AS_Expression -> AS_InfixOp -> [AS_Expression] -> AS_Expression
    buildInfixTree _ _ [] =
      error "The impossible happened!" -- the aoExpr case mL check for [(op,f)] guards against this case! it's safe.
    buildInfixTree e op [x] =
      AS_InfixOP (infoE e) op e x
    buildInfixTree e op (x:xs) =
      AS_InfixOP (infoE e) op e (buildInfixTree x op xs)
    sepBy1NotLeft :: Int -> GenParser Char st a -> GenParser Char st (a, [(AS_InfixOp, a)])
    sepBy1NotLeft indent p =
        do{ x <- p
          ; xs <- many $ do{ pos <- getPosition
                           ; if sourceColumn pos >= indent
                               then do xx <-     (do { reservedOp "/\\"; return AS_AND })
                                             <|> (do { reservedOp "\\/"; return AS_OR })
                                       ff <- p
                                       return (xx, ff)
                               -- 'abort' the parsing of the infix operation in case the
                               -- 2nd expression is to the left of the first.
                               -- This is critical for siuations like in
                               --   https://github.com/ret/specifica/issues/4
                               -- where the /\ y must not associate with the y in \E ... y
                               --   \/ /\ \E y \in {TRUE}: y
                               --      /\ y
                               else pzero
                           }
          ; return (x,xs)
          }

-- Expression without binary /\ and \/
nonAOExpression :: TLAParser AS_Expression
nonAOExpression = do
  buildExpressionParser table basicExpr <?> "expression"

expressionNoAngularClose :: TLAParser AS_Expression
expressionNoAngularClose = buildExpressionParser table
                             basicExprNoAngularClose <?> "expression"

op_prefix :: AS_PrefixOp -> AS_InfoE -> AS_Expression -> AS_Expression
op_prefix op info e =
    let res = AS_PrefixOP info op e
     in --Trace.trace ("=Pre=> "++prettyPrintE res)
          res

op_postfix :: AS_PostfixOp -> AS_InfoE -> AS_Expression -> AS_Expression
op_postfix op info e =
    let res = AS_PostfixOP info op e
     in --Trace.trace ("=Post=> "++prettyPrintE res)
          res

-- op_postfixIgnore :: AS_InfoE -> AS_Expression -> AS_Expression
-- op_postfixIgnore _info e =
--     let res = e
--      in --Trace.trace ("=PostIgnore=> "++prettyPrintE res)
--           res

op_infix :: AS_InfixOp -> AS_InfoE
         -> AS_Expression  -> AS_Expression -> AS_Expression
op_infix op info a b =
    let res = AS_InfixOP info op a b
     in --Trace.trace ("=Infix=> "++prettyPrintE res)
          res

op_infixS :: AS_InfixOp -> (AS_InfoE, AS_Expression)
          -> AS_Expression  -> AS_Expression -> AS_Expression
-- FIXME MAKE THIS MORE GENERIC SO THAT IT WORKS FOR MORE THAN 2 DOTS
-- SHOULD BE A SIMPLE WALKER. SEE AST PICTURES BELOW.
op_infixS op (info, fal) a b =
    case b of
      {- CASE a[1] (0 dots)
      -}
      AS_CloseFunApp -> -- for a[1]
        let res = AS_InfixOP info op a fal -- drop b and use arg list
         in --Trace.trace ("=InfixS1=> "++prettyPrintE res)
              res

      {- CASE a[1].b (1 dot)
         Only dot has higher prec than [...]
         Below is the original AST:

              ** ORIGINAL AST **    -->    ** REWRITTEN AST **

                (FunApp info)
                      /\
                     a  (DOT info') <== b          (DOT info') <== res
                         /\                           /\
              CloseFunApp  b'    lhs ==> (FunApp info)  b'
                                              /\
                                             a  fal
      -}
      AS_InfixOP info' AS_DOT AS_CloseFunApp b' -> -- for a[1].b
        let lhs = AS_InfixOP info op a fal -- drop b and use arg list
            res = AS_InfixOP info' AS_DOT lhs b'  -- replace AS_CloseFunApp
         in --Trace.trace ("=InfixS2=> "++prettyPrintE res)
              res

      {- CASE a[1].b.c (2 dots)
         Only dot has higher prec than [...]
         Below is the original AST:

              ** ORIGINAL AST **    -->    ** REWRITTEN AST **

                 (FunApp info)
                      /\
                     a  (DOT i'') <== b           (DOT i'') <== res
                         /\                          /\
                 (DOT i')  (b'')     middle=>(DOT i')  b''
                      /\                        /\
         (CloseFunApp)  b'    lhs=>(FunApp info)  b'
                                        /\
                                       a  fal
      -}
      AS_InfixOP i'' AS_DOT (AS_InfixOP i' AS_DOT AS_CloseFunApp b') b'' ->
        let lhs = AS_InfixOP info op a fal -- drop b and use arg list
            middle = AS_InfixOP i' AS_DOT lhs b'
            res = AS_InfixOP i'' AS_DOT middle b''
         in --Trace.trace ("=InfixS3=> "++prettyPrintE res)
              res

      _ ->
          let AS_FunArgList _ l = fal
          in Trace.trace ("InfixS ERRROR (too many dots after [..], "++
                          "only 2 supported), UNEXPECTED a="++prettyPrintE a++
                          ", fal="++show (map prettyPrintE l)++
                          ", b="++prettyPrintE b) $
                         AS_Bool info False -- some value to typecheck ok

table :: OperatorTable Char PState AS_Expression
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
              ,binary "\\leq"      (op_infix  AS_LTEQ)     AssocNone
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

    -- Handle /\ and \/ outside the regular expression parser to
    -- address the column sensitive https://github.com/ret/specifica/issues/4.
    -- See aoExpr for new handling.
    -- Note, we keep \/ and /\ in the Pretty printer as is.
    --,{- 3/ 3-}[binary "\\/"        (op_infix  AS_OR)       AssocLeft
    --          ,binary "/\\"        (op_infix  AS_AND)      AssocLeft]

    ,{- 2/ 2-}[binary "~>"         (op_infix  AS_TildeGT)  AssocNone]
    ,{- 1/ 1-}[binary "=>"         (op_infix  AS_Implication) AssocNone
              ,prefix "INSTANCE"   (op_prefix AS_INSTANCE) -- ?? operator
              ,prefix "INSTANCE"   (op_prefix AS_LOCAL) ]  -- ?? operator
    ]

binary  name fun assoc = Infix (do{ p <- getPosition
                                  ; reservedOp name;
                                  ; return $ fun (mkInfo p) }) assoc
binaryS name fun assoc = Infix (do{ p <- getPosition
                                  ; reservedOp name;
                                  ; l<- try $ commaSep expressionNoAngularClose
                                  ; let fal = AS_FunArgList (mkInfo p) l
                                  ; return $ fun (mkInfo p, fal) }) assoc
prefix  name fun       = Prefix (do{ p <- getPosition
                                   ; reservedOp name
                                   ; return $ fun (mkInfo p) })
postfix name fun       = Postfix (do{ p <- getPosition
                                    ; reservedOp name
                                    ; return $ fun (mkInfo p) })

basicExpr :: TLAParser AS_Expression
basicExpr = choice $ basicExprListNoAngularClose ++
            [ do{ _ <- char ']'
                ; whiteSpace
                ; return AS_CloseFunApp
                } ]

basicExprNoAngularClose :: TLAParser AS_Expression
basicExprNoAngularClose = choice basicExprListNoAngularClose

basicExprListNoAngularClose =
    [ parens expression
    , letExpr
    , try squareExpr
    , try squareExprStutter
    , braceExpr
    , ifExpr
    , chooseExpr
    , quantifiedExpr
    , gtgtExpr
    , try operatorAppExpr
    , lambdaExpr
    , landExpr
    , lorExpr
    , number
    , caseExpr
    , do{ p <- getPosition
        ; s <- stringLiteral
        ; return $ AS_StringLiteral (mkInfo p) s
        }
    , boolean
    , qualident
    , do{ _ <- char '@'
        ; whiteSpace
        ; return AS_OldVal
        }
    ]

ifExpr :: TLAParser AS_Expression
ifExpr = do{ p <- getPosition
           ; reserved "IF"
           ; bool <- expression
           ; reserved "THEN"
           ; a <- expression
           ; reserved "ELSE"
           ; b <- expression
           ; return $ AS_IF (mkInfo p) bool a b
           }

caseArm :: TLAParser AS_CaseArm
caseArm = do{ p <- getPosition
            ; a <- expression
            ; reservedOp "->"
            ; b <- expression
            ; return $ case a of
                (AS_Ident _info [] "OTHER") -> AS_OtherCaseArm (mkInfo p) b
                _                           -> AS_CaseArm (mkInfo p) a b
            }

caseExpr :: TLAParser AS_Expression
caseExpr = do{ p <- getPosition
             ; reserved "CASE"
             ; arms <- sepBy1 (try caseArm) (reserved "[]")
             ; let (arms', otherarm) = case last arms of
                     (AS_OtherCaseArm _ _e) ->
                        (arms \\ [last arms], -- rem last
                         Just $ last arms)
                     (AS_CaseArm _ _a _b) -> (arms, Nothing)
                in return $ AS_Case (mkInfo p) arms' otherarm
             }

chooseExpr :: TLAParser AS_Expression
chooseExpr = do{ p <- getPosition
               ; reserved "CHOOSE"
               ; b <- quantifierBound1
               ; reservedOp ":"
               ; e <- expression
               ; return $ AS_Choose (mkInfo p) b e
               }

quantifiedExpr :: TLAParser AS_Expression
quantifiedExpr = do{ p <- getPosition
                   ; t <-     do{ reserved "\\A"; return AS_All }
                          <|> do{ reserved "\\E"; return AS_Exist }
                   ; b <- commaSep1 quantifierBound
                   ; reservedOp ":"
                   ; e <- expression
                   ; return $ AS_Quantified (mkInfo p) t b e
                   }

operatorAppExpr :: TLAParser AS_Expression
operatorAppExpr = do{ p <- getPosition
                    ; qname <- qualident
                    ; l <- parens $ commaSep expression -- FIXME too liberal
                    ; return $ AS_OpApp (mkInfo p) qname l
                    }

lambdaExpr :: TLAParser AS_Expression
lambdaExpr = do{ p <- getPosition
               ; reserved "LAMBDA"
               ; l <- commaSep1 qualident
               ; reservedOp ":"
               ; e <- expression
               ; return $ AS_Lambda (mkInfo p) l e
               }

squareExpr :: TLAParser AS_Expression -- left factor [ ...
squareExpr = squares $  choice [ -- use try to tell -> from |->
                          try functionType            -- [ 1..3 -> 4..5]
                        , try recordExcept
                        , try recordFunction
                        , try recordType
                        , try quantifierBoundFunction
                        ]

squareExprStutter :: TLAParser AS_Expression -- left factor [...]_...
squareExprStutter = do{ e <- squares expression
                      ; _ <- char '_'
                      ; st <-    do{ p <- getPosition
                                   ; i <- identifier
                                   ; return $ mkIdent p [] i }
                             <|> gtgtExpr -- tuple
                      ; return $ AS_Stutter e st
                      }

quantifierBoundFunction :: TLAParser AS_Expression -- [x \in F |-> cs[x].foo]
quantifierBoundFunction = do{ p <- getPosition
                            ; b <- commaSep quantifierBound
                            ; reservedOp "|->"
                            ; e <- expression
                            ; return $
                                AS_QuantifierBoundFunction (mkInfo p) b e
                            }

recordFunction :: TLAParser AS_Expression -- [a |-> x+1, b |-> y]
recordFunction = do{ p <- getPosition
                   ; l <- commaSep $ do{ i <- try identifier
                                         ; reservedOp "|->"
                                         ; e <- expression
                                         ; return $ AS_MapTo (AS_Field i) e
                                         }
                   ; return $ AS_RecordFunction (mkInfo p) l
                   }

recordExceptDot :: TLAParser AS_ExceptNav
recordExceptDot = do{ _ <- dot
                    ; -- note, no whitespace allowed after dot
                    ; n <- identifier
                    ; return $ AS_ExceptNavField (AS_Field n)
                    }

recordExceptArr :: TLAParser AS_ExceptNav
recordExceptArr = do{ l <- squares $ commaSep expression
                    ; return $ AS_ExceptNavApp l
                    }

recordExcept :: TLAParser AS_Expression
recordExcept = do{ e <-     expression
                        <|> do{ _ <- char '@'; whiteSpace; return AS_OldVal }
                 ; reserved "EXCEPT" -- FIXME BOOK does not allow [x].y! REPORT
                 ; l <- commaSep $
                   do{ _ <- char '!'
                     ; l <- many1 $ recordExceptDot <|> recordExceptArr
                     ; reservedOp "="
                     ; val <- expression
                     ; return $ AS_ExceptAssignment l val
                     }
                 ; return $ AS_Except e l
                 }

functionType :: TLAParser AS_Expression -- [S -> T]
functionType = do{ p <- getPosition
                 ; a <- expression
                 ; reservedOp "->"
                 ; b <- expression
                 ; return $ AS_FunctionType (mkInfo p) a b
                 }

recordType :: TLAParser AS_Expression -- [a: Foo, b: X \cup Y]
recordType = do{ p <- getPosition
               ; l <- commaSep $ do{ p <- getPosition
                                   ; i <- identifier
                                   ; reservedOp ":"
                                   ; e <- expression
                                   ; return $ AS_RecordElementType (mkInfo p)
                                                (AS_Field i) e
                                   }
               ; return $ AS_RecordType (mkInfo p) l
               }

{-- FIXME DELETE
funArgList :: TLAParser [AS_Expression] -- [a,b][c] is two FunArgLists
funArgList = do{ p <- getPosition
             ; l <- many1 $ try $ squares $ commaSep expression
             ; Trace.trace ("==> "++show l) $ return ()
             ; return $ map (\ll -> AS_FunArgList (mkInfo p) ll) l
             }
--}

braceExpr :: TLAParser AS_Expression -- left factor { ...
braceExpr = braces $ choice [
                       try $ do{ p <- getPosition
                               ; q <- quantifierBound1
                               ; reservedOp ":"
                               ; e <- expression
                               ; return $ AS_SetComprehension (mkInfo p) q e
                               }
                     , try $ do{ p <- getPosition
                               ; e <- expression
                               ; reservedOp ":"
                               ; q <- quantifierBound
                               ; return $ AS_SetGeneration (mkInfo p) e q
                               }
                     ,       do{ p <- getPosition
                               ; l <- commaSep expression
                               ; return $ AS_DiscreteSet (mkInfo p) l
                               }
                     ]

gtgtExpr :: TLAParser AS_Expression -- left factor << ...
gtgtExpr = try $ do{ p <- getPosition
                   ; (between (symbol "<<") (symbol ">>") $
                        do{ l <- commaSep expression
                          ; return $ AS_Tuple (mkInfo p) l
                          })
                   }

letExpr :: TLAParser AS_Expression
letExpr = do{ p <- getPosition
            ; reserved "LET"
            ; l <- sepBy              -- USE unit?
                     (choice [
                        try operatorDef
                      , try funDef
                      , recursive
                      ])
                     whiteSpace
            ; reserved "IN"
            ; e <- expression
            ; return $ AS_Let (mkInfo p) l e
            }

sepBy1At :: Int -> GenParser tok st a -> GenParser tok st sep
         -> GenParser tok st [a]
sepBy1At indent p sep =
    do{ x <- p
      ; xs <- many $ do{ pos <- getPosition
                       ; if sourceColumn pos == indent -- require precise alignment
                         then sep >> p
                         else pzero
                       }
      ; return (x:xs)
      }

landExpr :: TLAParser AS_Expression
landExpr = do{ p <- getPosition
             ; reservedOp "/\\"
             ; updateState $ -- update state, so p is visible in expr
                 \s -> pushIndent s $ sourceColumn p
             ; le <- sepBy1At (sourceColumn p)
                       (do{ e <- try expression
                          ; return e
                          })
                       (reserved "/\\")
             ; updateState $ \s -> let (_, s') = popIndent s in s'
             ; return $ AS_LAND (mkInfo p) le
             }

lorExpr :: TLAParser AS_Expression
lorExpr = do{ p <- getPosition
             ; reservedOp "\\/"
             ; updateState $ -- update state, so p is visible in expr
                 \s -> pushIndent s $ sourceColumn p
             ; le <- sepBy1At (sourceColumn p)
                       (do{ e <- try expression
                          ; return e
                          })
                       (reserved "\\/")
             ; updateState $ \s -> let (_, s') = popIndent s in s'
             ; return $ AS_LOR (mkInfo p) le
             }

boolean :: TLAParser AS_Expression
boolean =     do{ p <- getPosition
                ; reserved "TRUE"
                ; return $ AS_Bool (mkInfo p) True
                }
          <|> do{ p <- getPosition
                ; reserved "FALSE"
                ; return $ AS_Bool (mkInfo p) False
                }

number :: TLAParser AS_Expression
number = do{ p <- getPosition
           ; n <- natural
           ; return $ AS_Num (mkInfo p) (fromInteger n)
           }

-------------------------------------------------------------------------------
mkCfgInfo :: SourcePos -> CFG_Info
mkCfgInfo p = p

cfgspec :: TLAParser CFG_Config
cfgspec = do { whiteSpace
             ; name <- option Nothing cfgheader
             ; stmts <- cfgstatement_list
             ; return $ CFG_Config name stmts
             }

cfgheader :: TLAParser (Maybe String)
cfgheader = do{ reserved "SPECIFICATION"
              ; i <- identifier
              ; return $ Just i
              }

cfgstatement_list :: TLAParser [CFG_Statement]
cfgstatement_list = sepBy (choice [
                             cfgconstant_def
                           , cfginvariant
                           , cfgproperty
                           , cfgview
                           , cfgsymmetry
                           ])
                    whiteSpace

cfginvariant :: TLAParser CFG_Statement
cfginvariant = do{ p <- getPosition
                 ; reserved "INVARIANT" <|> reserved "INVARIANTS"
                 ; l <- sepBy cfgident whiteSpace
                 ; return $ CFG_Invariant (mkCfgInfo p) l
                 }

cfgproperty :: TLAParser CFG_Statement
cfgproperty = do{ p <- getPosition
                ; reserved "PROPERTY" <|> reserved "PROPERTIES"
                ; l <- sepBy cfgident whiteSpace
                ; return $ CFG_Property (mkCfgInfo p) l
                }

cfgview :: TLAParser CFG_Statement
cfgview = do{ p <- getPosition
            ; reserved "VIEW"
            ; i <- cfgident
            ; return $ CFG_View (mkCfgInfo p) i
            }

cfgsymmetry :: TLAParser CFG_Statement
cfgsymmetry = do{ p <- getPosition
                ; reserved "SYMMETRY"
                ; i <- cfgident
                ; return $ CFG_Symmetry (mkCfgInfo p) i
                }

cfgconstant_def :: TLAParser CFG_Statement
cfgconstant_def = do{ p <- getPosition
                    ; reserved "CONSTANT" <|> reserved "CONSTANTS"
                    ; l <- sepBy cfgconstant whiteSpace
                    ; return $ CFG_ConstantDef (mkCfgInfo p) l
                    }

cfgconstant :: TLAParser CFG_ConstantEntry
cfgconstant = do{ p <- getPosition
                ; i <- cfgident
                ; e <-     do { reservedOp "="
                              ; v <- cfgvalue
                              ; return $ CFG_Assignment (mkCfgInfo p) i v
                              }
                       <|> do { reservedOp "<-"
                              ; b <- cfgident
                              ; return $ CFG_Subst (mkCfgInfo p) i b
                              }
                ; return e
                }

cfgident :: TLAParser CFG_Ident
cfgident = do{ p <- getPosition
             ; i <- identifier
             ; return $ CFG_Ident (mkCfgInfo p) i
             }

cfgvalue :: TLAParser CFG_Value
cfgvalue = do{ p <- getPosition
             ; choice [
                    cfgboolean
                  , do{ i <- identifier
                      ; return $ CFG_Atom (mkCfgInfo p) i
                      }
                  , do{ n <- natural
                      ; return $ CFG_Int (mkCfgInfo p) (fromInteger n)
                      }
                  , do{ s <- stringLiteral
                      ; return $ CFG_StringLiteral (mkCfgInfo p) s
                       }
                  , do{ p <- getPosition
                      ; s <- braces (commaSep cfgvalue)
                      ; return $ CFG_Set (mkCfgInfo p) (Set.fromList s)
                      }]
             }

cfgboolean :: TLAParser CFG_Value
cfgboolean =     do{ p <- getPosition
                   ; reserved "TRUE"
                   ; return $ CFG_Bool (mkCfgInfo p) True
                   }
             <|> do{ p <- getPosition
                   ; reserved "FALSE"
                   ; return $ CFG_Bool (mkCfgInfo p) False
                   }
-------------------------------------------------------------------------------
lexer = lexer0{P.reservedOp = rOp}
          where
            lexer0      = P.makeTokenParser tladef
            resOp0      = P.reservedOp lexer0
            resOp1 name = do _ <- string name -- \in
                             notFollowedBy letter <?> ("end of " ++ show name)
            resOp2 name = do _ <- string name -- \ (set difference)
                             notFollowedBy (letter <|> char '/') <?>
                                               ("end of " ++ show name)
            resOp3 name = do _ <- string name -- \/ (or)
                             return ()
            ------
            resOp4 name = do _ <- string name -- ~>
                             return ()
            resOp5 _name = do _ <- char '~' -- ~ (not)
                              return ()
            ------
            resOp6 name = do _ <- string name -- ==
                             notFollowedBy (char '=' {-??-} <|> char '>') <?>
                                               ("end of " ++ show name)
            resOp7 name = do _ <- string name -- =>
                             return ()
            resOp8 name = do _ <- string name -- =
                             notFollowedBy (char '=' <|> char '>') <?>
                                               ("end of " ++ show name)
            ------
            resOpLT0 name = do _ <- string name -- <>
                               notFollowedBy (char '>' {-??-} <|> char '=') <?>
                                                 ("end of " ++ show name)
            resOpLT1 name = do _ <- string name -- <=
                               return ()
            resOpLT2 name = do _ <- string name -- <
                               notFollowedBy (char '=' <|> char '>') <?>
                                                 ("end of " ++ show name)
            ------
            resOpAngular name = do _ <- string name -- []
                                   notFollowedBy (char ']') <?>
                                                ("end of " ++ show name)
            ------
            rOp name = lexeme $ try $
                          case name of
                            ('\\':cs@(_:_))
                                | cs == "/"      -> resOp3 name -- \/
                                | all isAlpha cs -> resOp1 name -- \in, etc.
                            "\\" -> resOp2 name                 -- \ (set -)
                            ------
                            "~>" -> resOp4 name
                            "~"  -> resOp5 name
                            ------
                            "==" -> resOp6 name -- ==
                            "=>" -> resOp7 name -- =>
                            "="  -> resOp8 name -- =
                            ------
                            "<>" -> resOpLT0 name -- <>
                            "<=" -> resOpLT1 name -- <=
                            "<"  -> resOpLT2 name -- <
                            ------
                            "[]" -> resOpAngular name
                            ------
                            -- pre/postfix operator, special handling to cover
                            -- cases where operator is immediately followed by
                            -- infix (e.g. cs'.x), or expression
                            -- (e.g. UNCHANGED<<cs>>)
                            "'" -> resOp1 name
                            "UNCHANGED" -> resOp1 name
                            "DOMAIN" -> resOp1 name
                            "SUBSET" -> resOp1 name
                            "UNION" -> resOp1 name
                            _ -> resOp0 name
            -- isAlphaOrUnderscore c = isAlpha c || c == '_'
            lexeme p = do { x <- p; P.whiteSpace lexer0; return x }

tladef = emptyDef {
  P.commentStart    = "(*"
, P.commentEnd      = "*)"
, P.commentLine     = "\\*"
, P.nestedComments  = True -- for PlusCal, e.g. Examples/specification/TLC/TLCMC.tla
, P.identStart      = letter <|> char '_'
-- TLC EXTENSION, allow @ in identifiers for 'short' pc guards (when a@b ...)
, P.identLetter     = alphaNum <|> char '_' <|> char '@'
--, P.identLetter     = alphaNum <|> char '_'
, P.opStart         = oneOf $ nub $
                        map (\s -> head s) $ P.reservedOpNames tladef
, P.opLetter        = oneOf symbs
, P.reservedOpNames = [ -- prefix operators
                        "SUBSET"
                      , "DOMAIN"
                      , "UNION"
                      , "UNCHANGED"
                      , "INSTANCE"
                      , "[]"
                      , "<>"
                      , "~"
                        -- postfix operators, REMEMBER to add to 'lexer' also!
                      , "'"
                        -- infix operators
                      , "=", "#", ":>", "@@"
                      , "\\"
                      , "."
                      , ".."
                      , "+", "-", ">", "<", "<=", ">="
                        -- general
                      , "=="
                      , "->"
                      , "~>"
                      , "|->"
                      , "=>"
                      , ":"
                      , "/="
                      , "\\in"
                      , "\\leq"
                      , "\\notin"
                      , "\\times"
                      , "\\o", "\\circ"
                      , "\\subseteq", "\\cup", "\\cap"
                      , "\\X", "\\div"
                      , "\\/", "/\\"
                      , "<-"
                      ]
, P.reservedNames   = [ "CONSTANT", "CONSTANTS"
                      , "RECURSIVE"
                      , "VARIABLE", "VARIABLES"
                      , "ASSUME"
                      , "LET", "IN", "IF", "THEN", "ELSE", "CHOOSE"
                      , "LAMBDA"
                      ,  "EXCEPT", "!.", "CASE", "[]"
                      -- operator synonyms
                      , "\\A", "\\E"
                      , "TRUE", "FALSE"
                      -- CONFIG CFG FILE
                      , "SPECIFICATION"
                      , "INVARIANT", "INVARIANTS"
                      , "PROPERTY", "PROPERTIES"
                      , "SYMMETRY"
                      , "LOCAL"
                        {- ,"VIEW" HACK FIXME, took out to allow for VIEW(R) -}
                        {- where VIEW is part of an expression (short.hs)    -}
                      ]
, P.caseSensitive   = True }
  where
    symbs = filter (not . isAlpha) . concat $ P.reservedOpNames tladef

dot             = P.dot lexer
parens          = P.parens lexer
braces          = P.braces lexer
squares         = P.squares lexer
-- semiSep         = P.semiSep lexer
-- semiSep1        = P.semiSep1 lexer
commaSep        = P.commaSep lexer
commaSep1       = P.commaSep1 lexer
-- brackets        = P.brackets lexer
whiteSpace      = P.whiteSpace lexer
symbol          = P.symbol lexer
identifier      = P.identifier lexer
reserved        = P.reserved lexer
reservedOp      = P.reservedOp lexer
-- integer         = P.integer lexer
natural         = P.natural lexer
-- charLiteral     = P.charLiteral lexer
stringLiteral   = P.stringLiteral lexer
