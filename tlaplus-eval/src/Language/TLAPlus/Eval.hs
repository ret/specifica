module Language.TLAPlus.Eval where

import Prelude hiding ((<$>))
-- import Control.Monad.Error
import Control.Monad.Except
import Debug.Trace as Trace
import qualified Data.Set as Set (union, intersection,
                                  isSubsetOf, empty, size, insert)
import Data.Set as Set (fromList, toList, elems, (\\), member, isSubsetOf)
import Data.List as List (find, elemIndex, map, elem, lookup)
import qualified Data.Map as Map (union, lookup, insert, empty, singleton,
                                  keys, elems, fromList, toList)

import GHC.Stack

import Text.PrettyPrint.Leijen

import Language.TLAPlus.Syntax
import Language.TLAPlus.Pretty

eval :: [AS_Spec] -> CFG_Config -> ThrowsError [VA_Value]
eval specs cfg = evalReturnEnv specs cfg >>= \(_, vs) -> return vs

evalReturnEnv :: [AS_Spec] -> CFG_Config -> ThrowsError (Env, [VA_Value])
evalReturnEnv specs cfg =
    do{ let bindings = map (mkBinding ("foospec")) (cfg_constants cfg)
      ; env <- foldM (\e b -> bind e b) mkEmptyEnv bindings
      ; env' <- addBuiltIn env -- FIXME make module extend specific
      ; let units = concat $ map unitDef specs
      ; (env'', vs) <- foldM evalUnitT (env', []) units
      ; return (env'', vs)
      }
    where mkBinding :: String -> (CFG_Ident, CFG_Value)
                    -> (AS_Expression, VA_Value)
          mkBinding q (i,v) = (toASIdent q i, toASValue v)
          toASIdent :: String -> CFG_Ident -> AS_Expression
          toASIdent _q (CFG_Ident _ i) = -- FIXME what about q?
              AS_Ident (mkDummyInfo "a-cfg-value") [] i
          toASValue :: CFG_Value -> VA_Value
          toASValue (CFG_Atom _ s) = VA_Atom s
          toASValue (CFG_Bool _ b) = VA_Bool b
          toASValue (CFG_Int _ i) = VA_Int i
          toASValue (CFG_StringLiteral _ s) = VA_String s
          toASValue (CFG_Set _ set) =
              let l = map toASValue (elems set)
               in VA_Set (Set.fromList l)

evalUnitT :: (Env, [VA_Value]) -> AS_UnitDef -> ThrowsError (Env, [VA_Value])
evalUnitT (env, vs) u =
    do{ -- Trace.trace ("\n\n== U ==\n" ++ (show u) ++
        --                "\n== ENV ==\n"++(show env)) $ return ()
      ; evalUnit (env, vs) u
      }

evalUnit :: (Env, [VA_Value]) -> AS_UnitDef -> ThrowsError (Env, [VA_Value])
evalUnit (env,vs) (AS_Separator _) = return $ (env, vs)
evalUnit (env,vs) (AS_Assume _uinfo e)  =
    evalET env e >>= \r -> return $ (env, vs++[r])
evalUnit (env,vs) (AS_ConstantDecl _uinfo l) =
    -- FIXME [...] constant "InitialUpReplicat in scope. [...]
    -- improve error message (say the symbol is missing in .cfg file)
    do{ mapM_ (\i -> lookupBinding i env "constant") l
      ; return (env, vs)
      }
-- FIXME for now only bind the name, so a ref succeeds
evalUnit (env,vs) (AS_VariableDecl _uinfo ids) =
    do{ env' <- foldM (\env id -> bind env (id, VA_Var Nothing)) env ids
      ; return $ (env', vs)
      }
evalUnit (env,vs) u@(AS_FunctionDef _ head _ _) =
    do{ v <- evalU env u
      ; env' <- bind env (head, v)
      ; return $ (env', vs)
      }
evalUnit (env,vs) u@(AS_OperatorDef _ (AS_OpHead head _) _) =
    do{ v <- evalU env u
      ; env' <- bind env (head, v)
      ; return $ (env', vs)
      }
evalUnit _acc u =
    throwError $ Default ("evalUnit, case missing "++show u)

-- FIXME see evalU (also used in evalE of AS_Let defs,
-- top level evalU should simply bind into the env and not yield a value

evalU :: Env -> AS_UnitDef -> ThrowsError VA_Value
evalU env u@(AS_FunctionDef infoU head quants e) =
    if length quants == 0 -- FIXME check to see if any free vars exist
    then evalET env e -- is a constant with free vars bound by env
    else return $ VA_FunctionDef (toInfoE infoU u) head quants e
evalU _env u@(AS_OperatorDef infoU h@(AS_OpHead _head _args) e) =
    -- NOTE even if the arity is 0, we don't evalE the expression since the
    -- expression e could include uninitialized variables (e.g. Foo == var).
    return $ VA_OperatorDef (toInfoE infoU u) h e
evalU _ _ = error "unspecified"

evalET :: Env -> AS_Expression -> ThrowsError VA_Value
evalET env e =
    if False -- FIXME add log (list of strings) to ThrowsError monad
    then do{ res <- evalE env e
           ; Trace.trace ("[["++show (prettyPrintE e)++
                          "]] = "++show (prettyPrintVA res)) $ return ()
           ; return res
           }
    else evalE env e

evalE :: Env -> AS_Expression -> ThrowsError VA_Value
evalE env i@(AS_Ident _info _quallist _name) = --FIXME lookup proper env for qual
    lookupBinding i env "identifier" >>= \e -> case e of
{-
        -- FIXME check bounds on function invocation!
      (VA_FunctionDef _info _head _bounds opExpr) -> evalET env opExpr
        -- FIXME should I call evalOperator here?
        -- FIXME do I still need evalE AS_OpApp?
-}
      (VA_Var v) -> case v of
                      Nothing -> return $ VA_Var (Just (VA_String "undefined")) -- throwError $ RdBeforeWr i
                      Just v' -> return v'
      (VA_OperatorDef _info _head opExpr) -> evalET env opExpr
      _ -> return e

evalE env _e@(AS_FunArgList _ l) =
    VA_FunArgList `liftM` mapM (\e -> evalET env e) l

evalE env e@(AS_OpApp _info qname exprargs) = -- FIXME cross check w/ Ident hdl
    do{ op <- lookupBinding qname env "operator"
      ; case op of
          (VA_OperatorDef _info (AS_OpHead _name argnames) expr) ->
              evalOperator(env, argnames, exprargs, expr)
          _ ->
              throwError $ Default ("FIXME - op="++show op++"////"++show e)
      }
evalE env _e@(AS_FunctionType _info a b) =
    do{ a' <- evalET env a
      ; b' <- evalET env b
      ; return $ VA_FunType a' b'
      }
evalE env e@(AS_PrefixOP _info op a) = (evalOpPrefix env op) e a
evalE env e@(AS_PostfixOP _info op a) = (evalOpPostfix env op) e a
evalE env e@(AS_InfixOP _info op a b) = (evalOpInfix env op) e a b
evalE env _e@(AS_Let _info units expr) =
    do{ env' <- foldM (\env u -> evalU env u >>= \v ->
                         bind env (nameU u, v)
                      ) env (filterUnits units)
      ; evalET env' expr
      }
    where filterUnits = filter (\u -> case u of
            (AS_FunctionDef _ _ _ _) -> True
            (AS_OperatorDef _ _ _) -> True
            _ -> False)

evalE env e@(AS_IF _info b x y) =
    evalET env b >>= \res -> case res of
                               (VA_Bool True)  -> evalET env x
                               (VA_Bool False) -> evalET env y
                               _ -> throwError $
                                 IllegalType e b res TY_Bool "conditional"
evalE env (AS_DiscreteSet _info  le) =
    VA_Set `liftM` Set.fromList `liftM` mapM (\e -> evalET env e) le
evalE env (AS_RecordFunction _info l) =
    foldM (\(VA_Rec map) (AS_MapTo (AS_Field k) e) -> evalET env e >>= \v ->
                return $ VA_Rec $ Map.insert (VA_String k) v map)
          (VA_Rec Map.empty) l
evalE env e@(AS_Quantified _info qkind [qbound] expr) = -- FIXME singleton!
    do{ let (AS_QBoundN [var] bexpr) = qbound
      ; elements <- enumElements env e bexpr
      ; envs <- mapM (\v -> bind env (var, v)) elements
      ; bvs <- mapM (\env ->
                 case evalET env expr of
                     Right (VA_Bool b) -> return $ VA_Bool b
                     Right other -> throwError $
                       IllegalType e expr other TY_Bool "quantifier"
                     Left (KeyNotFound _ _ _ _ _) -> -- ignore "not found"
                       return $ VA_Bool False
                     Left msg -> throwError msg
                 ) envs
      ; let b = (quant qkind) (\(VA_Bool v) -> v) bvs
      ; return $ (VA_Bool b)
      }
    where quant AS_All   = all
          quant AS_Exist = any
evalE env e@(AS_QuantifierBoundFunction _info [qbound] expr) = --FIXME singleton!
    do{ let (AS_QBoundN [var] qexpr) = qbound
      ; elements <- enumElements env e qexpr
      ; envs <- mapM (\v -> bind env (var, v) >>= \env' ->
                        return (v, env')) elements
      ; foldM (\(VA_Map map) (argv,env) -> evalET env expr >>= \resv ->
                         return $ VA_Map $ Map.insert argv resv map
                    ) (VA_Map Map.empty) envs
      }
evalE env e@(AS_Choose _info (AS_QBound1 qvar qexpr) bexpr) =
    do{ elements <- enumElements env e qexpr
      ; v <- findM (\qvalue ->
               bind env (qvar, qvalue) >>= \env' ->
                   case evalET env' bexpr of
                     Right (VA_Bool b) -> return b
                     Right other -> throwError $
                       IllegalType e bexpr other TY_Bool "CHOOSE"
                     Left (KeyNotFound _ _ _ _ _) -> -- ignore "not found"
                       return False
                     Left msg -> throwError msg
             ) elements
      ; case v of
          Just v' -> return v'
          Nothing -> throwError $ ChooseNoMatch qvar qexpr bexpr elements env
      }
    where findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
          findM _ [] = return Nothing
          findM f (x:xs) = do { b <- f x
                              ; if b then return (Just x) else findM f xs }
evalE env (AS_Tuple _info exprs) =
    VA_Seq `liftM` mapM (evalET env) exprs
evalE env e@(AS_LAND _info exprs) = list_bool e env all exprs
evalE env e@(AS_LOR _info exprs) = list_bool e env any exprs
evalE _env (AS_Num _info n) = return $ VA_Int n
evalE _env (AS_Bool _info b) = return $ VA_Bool b
evalE _env (AS_StringLiteral _info s) = return $ VA_String s
evalE env (AS_RecordType _info l) =
    foldM (\(VA_RecType map) -- FIXME Austin, do not enum the elements!
            (AS_RecordElementType _info (AS_Field f) ve) ->
                evalET env ve >>= \v ->
                    return $ VA_RecType $ Map.insert (VA_String f) v map)
          (VA_RecType Map.empty) l
evalE env e@(AS_SetComprehension _info (AS_QBound1 qvar qexpr) expr) =
    do{ elements <- enumElements env e qexpr
      ; envs <- mapM (\v -> bind env (qvar, v) >>= \env' ->
                  return (v, env')) elements
      ; vs <- foldM (\acc (v, env) ->
                evalET env expr >>= \bv ->
                    case bv of
                           (VA_Bool True) -> return $ acc ++ [v]
                           (VA_Bool False) -> return acc
                           _ -> throwError $
                                   IllegalType e expr v TY_Bool "set comprehension"

              ) [] envs
      ; return $ VA_Set $ Set.fromList vs
      }
evalE env e@(AS_SetGeneration _info expr (AS_QBound1 qvar qexpr)) =
    do{ elements <- enumElements env e qexpr
      ; envs <- mapM (\v -> bind env (qvar, v)) elements
      ; vs <- mapM (\env -> evalET env expr >>= \v -> return v) envs
      ; return $ VA_Set $ Set.fromList vs
      }
evalE env e@(AS_Case _info arms other) =
    do{ arm <- findM (\(AS_CaseArm _info cexpr _vexpr) ->
                        evalET env cexpr >>= \bv ->
                          case bv of
                            (VA_Bool b) -> return b
                            _ -> throwError $
                                   IllegalType e cexpr bv TY_Bool "case condition"
                     ) arms
      ; case arm of
          Just (AS_CaseArm _info _bexpr vexpr) -> evalET env vexpr
          Just _ -> error "unspecified"
          Nothing ->
              case other of
                Just (AS_OtherCaseArm _info otherexpr) -> evalET env otherexpr
                Just (AS_CaseArm _ _ _) -> error "unspecified"
                Nothing -> throwError $ Default $
                  "No case arm matched and no 'other' clause specified."
      }
    where findM :: (Monad m) => (a -> m Bool) -> [a] -> m (Maybe a)
          findM _ [] = return Nothing
          findM f (x:xs) = do { b <- f x
                              ; if b then return (Just x) else findM f xs }
evalE env (AS_BIF mod name) =
    let Just fun = List.lookup (mod, name) bif_Table
     in fun env

-- add type otherwise
evalE _env e = throwError $ NoRuleError e -- FIXME for debugging only

toInfoE :: AS_InfoU -> AS_UnitDef -> AS_InfoE
toInfoE pos u = (pos, Just u, Nothing)

nameU :: AS_UnitDef -> AS_Expression
nameU (AS_FunctionDef _ head _ _) = head
nameU (AS_OperatorDef _ (AS_OpHead head _) _) = head
nameU _ = error "unspecified"

type Infix_Info = (Env, AS_Expression, AS_Expression, AS_Expression)
type Prefix_Info = (AS_Expression, AS_Expression)
type Postfix_Info = (AS_Expression, AS_Expression)


eval_dot op loc@(env, _parent, _, b) va =
  if op == AS_DOT
    -- FIXME perhaps also check to make sure "name" is not
    -- bound!
  then case b of
         (AS_Ident _info [] name) ->
           op_dot loc va (VA_String name)
         _ ->
           do{ vb <- (evalET env b)
             ; infix_op op loc va vb
             }
  else do{ vb <- (evalET env b)
         ; infix_op op loc va vb
         }

evalOpInfix :: Env
            -> AS_InfixOp -> AS_Expression -> AS_Expression -> AS_Expression
            -> ThrowsError VA_Value
-- FIXME kramer@acm.org reto --
-- crux is that whenever such an assignment of the form (foo' = ...) is found,
-- we must be able to transfer the value to the outside of the enclosing top
-- level action.
-- This brings up the question of where the top level stops
-- (e.g. Next = A \/ B), or at A and B and the \/ takes it from there?
-- What is the right monad to combine these sets of values (a value being the
-- record of all variables in the TLA+ spec).
evalOpInfix env _op@AS_EQ _parent
            (AS_PostfixOP _ AS_Prime a@(AS_Ident _ [] _i)) b =
    evalET env a >>= \case
      (VA_Var _) -> do{ vb <- (evalET env b)
                      ; env' <- replace env (a, vb)
                      ; Trace.trace (ppEnv env') $ return vb
                      }
      _ -> error "unspecified"
evalOpInfix env op@AS_EQ parent a@(AS_Ident _ [] _i) b =
    evalET env a >>= \va -> case va of
      (VA_Var _) -> do{ vb <- (evalET env b)
                      ; env' <- replace env (a, vb)
                      ; Trace.trace (ppEnv env') $ return vb
                      }
      _ ->
        case va of
          {-- In TLA+ r.a and r."a" (so is r["a"], but r[a] is not) are the
          same if r is a record. All the quoted forms work well without
          special handling, but in r.a, it's important to not
          treat 'a' as an identifier (unbound) if r is a record.  Here
          we handle the r.a case.  The r[a] case is different in that a is
          treated as an identifier and hence needs no special handling. --}
          (VA_Rec _m) -> eval_dot op (env, parent, a, b) va
            -- FIXME, does TLA+ allow set comprehension over rec types?
            --   LET B == {1,2} S == {[t: {"S1"}, b: B], [t: {"S2"}, b: B]}
            --    IN { a.t : a \in S }
          (VA_RecType _m) -> eval_dot op (env, parent, a, b) va
          _ -> do{ vb <- (evalET env b)
                         ; infix_op op (env, parent, a, b) va vb
                         }
evalOpInfix env op parent a b =
  do{ va <- (evalET env a)
    ; case va of
        {-- In TLA+ r.a and r."a" (so is r["a"], but r[a] is not) are the
        same if r is a record. All the quoted forms work well without
        special handling, but in r.a, it's important to not
        treat 'a' as an identifier (unbound) if r is a record.  Here
        we handle the r.a case.  The r[a] case is different in that a is
        treated as an identifier and hence needs no special handling. --}
        (VA_Rec _m) -> eval_dot op (env, parent, a, b) va
          -- FIXME, does TLA+ allow set comprehension over rec types?
          --   LET B == {1,2} S == {[t: {"S1"}, b: B], [t: {"S2"}, b: B]}
          --    IN { a.t : a \in S }
        (VA_RecType _m) -> eval_dot op (env, parent, a, b) va
        _ -> do{ vb <- (evalET env b)
                       ; infix_op op (env, parent, a, b) va vb
                       }
    }

infix_op :: AS_InfixOp -> Infix_Info ->
            VA_Value -> VA_Value -> ThrowsError VA_Value
infix_op op i v w =
  case lookup op infix_op_table of
    Just f -> f i v w
    Nothing -> throwError $ UnknownOperatorError
                 ("INTERNAL ERROR: don't know how to evaluate " ++
                  "(" ++ show op ++ ")")

evalOpPrefix :: Env
             -> AS_PrefixOp -> AS_Expression  -> AS_Expression
             -> ThrowsError VA_Value
evalOpPrefix env op parent e = evalET env e >>= prefix_op op (parent, e)
  where prefix_op :: AS_PrefixOp -> Prefix_Info ->
                     VA_Value -> ThrowsError VA_Value
        prefix_op op i v =
            case lookup op prefix_op_table of
              Just f -> f i v
              Nothing -> throwError $ UnknownOperatorError
                           ("INTERNAL ERROR: don't know how to evaluate " ++
                            "(" ++ show op ++ ")")

evalOpPostfix :: Env
             -> AS_PostfixOp -> AS_Expression  -> AS_Expression
             -> ThrowsError VA_Value
evalOpPostfix env op parent e = evalET env e >>= postfix_op op (parent, e)
  where postfix_op :: AS_PostfixOp -> Postfix_Info ->
                     VA_Value -> ThrowsError VA_Value
        postfix_op op i v =
            case lookup op postfix_op_table of
              Just f -> f i v
              Nothing -> throwError $ UnknownOperatorError
                           ("INTERNAL ERROR: don't know how to evaluate " ++
                            "(" ++ show op ++ ")")

infix_op_table ::
    [(AS_InfixOp, Infix_Info -> VA_Value -> VA_Value -> ThrowsError VA_Value)]
infix_op_table =
  [ (AS_Plus, (op_plus)), (AS_Minus, (op_minus)),
    (AS_Mult, (op_mult))
  , (AS_Cup, (op_cup)), (AS_Cap, (op_cap))
  , (AS_EQ, (op_eq)), (AS_NEQ, (op_neq)), (AS_LT, (op_lt)),
    (AS_LTEQ, (op_lteq)), (AS_GT, (op_gt))
  , (AS_COLONGT, (op_colongt)), (AS_ATAT, (op_atat))
  , (AS_DOTDOT, (op_dotdot))
  , (AS_DOT, (op_dot))
  , (AS_SetMinus, (op_setminus))
  , (AS_SubsetEq, (op_subseteq))
  , (AS_In, (op_in))
  , (AS_NotIn, (op_notin))
  , (AS_Times, (op_times))
  , (AS_AND, (op_and))
  , (AS_OR, (op_or))
  , (AS_FunApp, (op_funapp))
  ]

prefix_op_table :: [(AS_PrefixOp,
                     Prefix_Info -> VA_Value -> ThrowsError VA_Value)]
prefix_op_table =
  [ (AS_SUBSET, (op_subset))
  , (AS_UNION,  (op_union))
  , (AS_DOMAIN, (op_domain))
  , (AS_Not, (op_not))
  ]

postfix_op_table :: [(AS_PostfixOp,
                     Postfix_Info -> VA_Value -> ThrowsError VA_Value)]
postfix_op_table =
  [ (AS_Prime, (op_prime)) ]

-- infix
op_plus _i (VA_Int a) (VA_Int b) = return $ VA_Int $ a + b
op_plus i va vb = throwError $ TypeMissmatch i va vb [TY_Int]

op_minus _i (VA_Int a) (VA_Int b) = return $ VA_Int $ a - b
op_minus i va vb = throwError $ TypeMissmatch i va vb [TY_Int]

op_mult _i (VA_Int a) (VA_Int b) = return $ VA_Int $ a * b
op_mult i va vb = throwError $ TypeMissmatch i va vb [TY_Int]

op_cup _i (VA_Set a) (VA_Set b) = return $ VA_Set $ Set.union a b
op_cup _i (VA_Set a) b = return $ VA_Set $ Set.insert b a
op_cup _i a@(VA_RecType _) b@(VA_RecType _) =
    return $ VA_Set $ Set.fromList [a,b]
op_cup i va vb = throwError $ TypeMissmatch i va vb [TY_Set]

op_cap _i (VA_Set a) (VA_Set b) = return $ VA_Set $ Set.intersection a b
op_cap i va vb = throwError $ TypeMissmatch i va vb [TY_Set]

-- FIXME see book, p. 264 (comparable)
op_eq _i (VA_Atom a) (VA_Atom b) = return $ VA_Bool $ a == b
op_eq _i (VA_Atom _a) _b = return $ VA_Bool False
op_eq _i _a (VA_Atom _b) = return $ VA_Bool False
op_eq i a b = if typeOf a == typeOf b
                then return $ VA_Bool $ a == b
                else throwError $ TypeMissmatch i a b $ [typeOf a, typeOf b]

op_neq _i (VA_Atom a) (VA_Atom b) = return $ VA_Bool $ a /= b
op_neq _i (VA_Atom _a) _b = return $ VA_Bool True
op_neq _i _a (VA_Atom _b) = return $ VA_Bool True
op_neq i a b = if typeOf a == typeOf b
                 then return $ VA_Bool $ a /= b
                 else throwError $ TypeMissmatch i a b $ [typeOf a, typeOf b]

op_lt _i (VA_Int a) (VA_Int b) = return $ VA_Bool $ a < b
op_lt i a b = throwError $ TypeMissmatch i a b $ [TY_Int]

op_lteq _i (VA_Int a) (VA_Int b) = return $ VA_Bool $ a <= b
op_lteq i a b = throwError $ TypeMissmatch i a b $ [TY_Int]

op_gt _i (VA_Int a) (VA_Int b) = return $ VA_Bool $ a > b
op_gt i a b = throwError $ TypeMissmatch i a b $ [TY_Int]

op_subseteq _i (VA_Set a) (VA_Set b) = return $ VA_Bool $ Set.isSubsetOf a b
op_subseteq i va vb = throwError $ TypeMissmatch i va vb [TY_Set]

op_setminus _i (VA_Set a) (VA_Set b) = return $ VA_Set $ a \\ b
op_setminus i va vb = throwError $ TypeMissmatch i va vb [TY_Int]

-- FIXME see book, p. 264
op_in i va (VA_Set s) =
    if s == Set.empty
    then return $ VA_Bool False
    else
      case head $ elems s of
        (VA_RecType _m) ->
            do{ bs <- mapM (\rectype -> op_in i va rectype)
                           (elems s)
              ; let b = any (\(VA_Bool b) -> b) bs
              ; return $ VA_Bool b
              }
        _ -> return $ VA_Bool (member va s)           -- Set of ...
op_in i (VA_Seq la) (VA_Seq lb) =
    do{ if length la == length lb -- FIXME need an allM function!
        then do{ bs <- mapM (\(as,bs) ->
                         op_in i as bs >>= \b -> case b of -- FIXME >>=typeChec
                           (VA_Bool _) -> return b
                           _ -> throwError $ Default "bool expected")
                       (zip la lb)
                ; return $ VA_Bool $ all (\(VA_Bool b) -> b) bs
                }
        else throwError $ Default ("comparing typles, size missmatch la=" ++
                                   show la ++ ", lb="++show lb)
      }
op_in i (VA_Seq l) (VA_SeqType tv) =
    do{ bs <- mapM (\v -> op_in i v tv) l
      ; return $ VA_Bool $ all (\(VA_Bool b) -> b) bs
      }
op_in i (VA_Map map) (VA_FunType a b) = -- [x |-> y] \in [X -> Y]
    -- FIXME, the FunType may have keys that the are not in the record, this
    -- is some ad-hoc subtype, but really I don't think TLA+ allows for it.
    do{ bs <- mapM (\(k,v) -> do{ VA_Bool kbool <- op_in i k a
                                ; VA_Bool vbool <- op_in i v b
                                ; return $ VA_Bool (kbool && vbool)
                                }) (Map.toList map)
      ; return $ VA_Bool $ all (\(VA_Bool b) -> b) bs
      }
op_in i (VA_Rec vmap) (VA_RecType tmap) =
    if Map.keys vmap == Map.keys tmap -- FIXME how about ordering?
    -- FIXME, the RecType may have keys that the are not in the record, this
    -- is some ad-hoc subtype, but really I don't think TLA+ allows for it.
    then do{ bs <- mapM (\k -> do{ let Just v = Map.lookup k vmap
                                 ; let Just t = Map.lookup k tmap
                                 ; VA_Bool bool <- op_in i v t
                                 ; return $ VA_Bool bool
                                 }) (Map.keys vmap)
           ; return $ VA_Bool $ all (\(VA_Bool b) -> b) bs
           }
    else return $ VA_Bool False -- incl. missmatch of record keys
op_in i va vb = throwError $ TypeMissmatch i va vb [TY_Set, TY_Seq]

op_notin i va vb = do{ VA_Bool res <- op_in i va vb
                     ; return $ VA_Bool (not res) }

op_colongt _i va vb = return $ VA_Map (Map.singleton va vb)

op_atat _i (VA_Map va) (VA_Map vb) = return $ VA_Map (Map.union va vb)
op_atat i va vb = throwError $ TypeMissmatch i va vb [TY_Map]

op_dotdot _i (VA_Int a) (VA_Int b) = return $
    VA_Set $ Set.fromList (map VA_Int [a .. b])
op_dotdot i va vb = throwError $ TypeMissmatch i va vb [TY_Int]

op_dot i va@(VA_Map a) vb =
    case Map.lookup vb a of
      Just v -> return v
      Nothing -> let (env, e, _, _) = i in throwError $
        KeyNotFound env e va vb "map field reference"
op_dot i va@(VA_Rec a) vb@(VA_String _) =
    case Map.lookup vb a of
      Just v -> return v
      Nothing -> let (env, e, _, _) = i in throwError $
        KeyNotFound env e va vb "record field reference"
op_dot i va@(VA_RecType a) vb@(VA_String _) = -- does TLA+ even allow this?
    case Map.lookup vb a of                   -- useful for Msg = [type: {"a"}
      Just v -> return v                      --   {m.t: m \in Msg}
      Nothing -> let (env, e, _, _) = i in throwError $
        KeyNotFound env e va vb "record type field reference"
op_dot _i va vb = throwError $
    Default ("Cannot apply (.) value "++show vb++" to subject "++show va)

op_times _i a b = return $ VA_Seq $ [a,b] -- tuples are seq of 2 elements

op_and _i (VA_Bool a) (VA_Bool b) = return $ VA_Bool (a && b)
op_and i va vb = throwError $ TypeMissmatch i va vb [TY_Bool]

op_or _i (VA_Bool a) (VA_Bool b) = return $ VA_Bool (a || b)
op_or i va vb = throwError $ TypeMissmatch i va vb [TY_Bool]

op_funapp i va argv@(VA_FunArgList argvaluelist) =
  let (env, e, _ea, _eb) = i in
  case va of
    (VA_Map map) -> mapLookup env argvaluelist map e va "map"
    (VA_Rec map) -> mapLookup env argvaluelist map e va "record"
    (VA_Set _) -> throwError $ FunAppIllegalOperand e va argv
    (VA_Seq l) -> let map = Map.fromList $ List.map (\(i,v) -> (VA_Int i, v))
                                                    (zip [1..length l] l)
                   in mapLookup env argvaluelist map e va "sequence"
    (VA_Int _) -> throwError $ FunAppIllegalOperand e va argv
    (VA_Bool _) -> throwError $ FunAppIllegalOperand e va argv
    (VA_String l) ->
       let map = Map.fromList $ List.map (\(i,v) -> (VA_Int i, VA_Char v))
                                         (zip [1..length l] l)
        in mapLookup env argvaluelist map e va "String"
    (VA_Char _) -> throwError $ FunAppIllegalOperand e va argv
    (VA_Atom _) -> throwError $ FunAppIllegalOperand e va argv
    (VA_FunctionDef _info _name qbounds expr) ->
       -- FIXME call this evalFunction(...)
       do{ let argnames = concat $ map (\(AS_QBoundN l _e) -> l) qbounds
         -- Bring the outer env into scope along with qbounds bound vars of the function.
         -- This is important to let Nat get passed into the function domain (n \in Nat), as in:
         --   LET Nat == 1..3
         --       factorial[n \in Nat] == IF n = 0 THEN 1 ELSE n * factorial[n-1]
         --    IN factorial[3]
         -- TODO: should emit a warning to the user when new bindings shadow old ones.
         ; envir' <- foldM (\env (n,v) -> bind env (n,v)) env
                           (zip argnames argvaluelist)
         ; mapM_ (\(AS_QBoundN [i] range) ->
             do{ let qe = (AS_InfixOP _info AS_In i range)
               ; evalET envir' qe >>= \r ->
                   case r of
                     VA_Bool True  -> return ()
                     VA_Bool False -> evalET envir' i >>= \val ->
                       throwError $ ValueOutOfBounds i range e val env
                     _ -> error "unspecified"
               }) qbounds
         ; if length argnames == length argvaluelist
           then evalET envir' expr
           else throwError $ Default ("Function application "++
                                      "error (arg length missmatch).")
         }
    _ -> error "unspecified"
  where mapLookup env keylist map e op kind =
          case keylist of
            [key] -> do{ case Map.lookup key map of
                                Just v -> return $ v
                                Nothing ->
                                    throwError $ KeyNotFound env e key op kind
                       }
            _ -> -- FIXME support list of arguments, foldl through
                throwError $ Default ("ERROR: only one argument supported "
                                      ++ (prettyPrintVA $ VA_Seq keylist))
op_funapp i va argv = throwError $ Default ("op_funapp i="++show i++
                                            ", // va="++prettyPrintVA va++
                                            ", // argv="++prettyPrintVA argv)

-- prefix
op_subset _i (VA_Set s) = let l   = Set.elems s
                              pl  = powerset $ l
                              pl' = map (\l -> VA_Set $ Set.fromList l) pl
                          in return $ VA_Set $ Set.fromList pl'
op_subset i v = let (e, a) = i
                 in throwError $ IllegalType e a v TY_Set "prefix expression"

op_domain _i (VA_Seq l) =
    return $ VA_Set $ Set.fromList $ map (\i -> VA_Int i) [1 .. length l]
op_domain _i (VA_Map m) =
    return $ VA_Set $ Set.fromList (Map.keys m)
op_domain i v = let (e, a) = i
                 in throwError $ IllegalType e a v TY_Seq "prefix expression"

op_union i (VA_Set s) =
    if s == Set.empty
      then return $ VA_Set Set.empty
      else
        case head $ elems s of
          (VA_Set _) ->
              do{ l <- foldM (\acc v ->
                    case v of
                      (VA_Set s') -> return $ acc ++ (elems s')
                      _ ->
                        let (e, a) = i
                         in throwError $
                              IllegalType e a v TY_Set "UNION expression"
                     ) [] (elems s)
                ; return $ VA_Set $ Set.fromList l
                }
          (VA_Seq _) ->
              do{ (x,y) <- foldM (\(xacc,yacc) v ->
                    case v of
                      (VA_Seq [x,y]) ->
                        return (xacc++[x], yacc++[y])
                      _ ->
                        let (e, a) = i
                         in throwError $
                              IllegalType e a v TY_Set "UNION expression"
                     ) ([],[]) (elems s)
                ; return $ VA_Seq [VA_Set $ Set.fromList x,
                                   VA_Set $ Set.fromList y]
                }
          other ->
              throwError $ Default ("Cannot eval UNION on Set of " ++
                                       prettyPrintVA other)
op_union i v = let (e, a) = i
                in throwError $ IllegalType e a v TY_Set "UNION expression"

op_not _i (VA_Bool b) = return $ VA_Bool$ not b
op_not i v = let (e, a) = i
              in throwError $ IllegalType e a v TY_Bool "negation expression"

-- postfix
op_prime _i a = return a -- FIXME, noop for now

-- FIXME I should not need this, rewrite the AST
op_closefunapp _i a =  return a -- noop

--
evalOperator(env, argnames, exprargs, expr) =
    if length argnames == 0
       then evalET env expr
       else if length argnames == length exprargs
            then do{ exprargvalues <- mapM (evalET env) exprargs
                   ; env' <- foldM (\env (n,v) -> bind env (n,v))
                                   env (zip argnames exprargvalues)
                   ; evalET env' expr
                   }
            else throwError $
              Default ("Operator application error (arg length missmatch)."++
                       "ARGNAMES=" ++ show argnames ++
                       "EXPRARGS=" ++ show exprargs ++
                       "EXPR = "++show expr)

enumElements :: Env -> AS_Expression -> AS_Expression -> ThrowsError [VA_Value]
enumElements env pe e =
    do{ evalET env e >>= \v -> case v of
          VA_Set s -> return $ elems s
          _ -> throwError $
            IllegalType pe e v TY_Set "element enumeration"
      }
    -- where enumRecTypeMap m = concat $
    --         List.map (\k -> case Map.lookup k m of
    --                      Just l ->
    --                         List.map (\v -> VA_Rec $ Map.fromList [(k,v)]) l
    --                    ) (Map.keys m)
    --       enumVA :: VA_Value -> [VA_Value] -- FIXME move into class/VA_Value
    --       enumVA (VA_Set s) = elems s

list_bool :: AS_Expression -> Env
          -> ((VA_Value -> Bool) -> [VA_Value] -> Bool)
          -> [AS_Expression]
          -> ThrowsError VA_Value
list_bool e env f exprs =
    do{ bvs <- mapM (\c -> evalET env c >>= \v -> case v of
                             (VA_Bool _) -> return v
                             _ -> throwError $
                               IllegalType e c v TY_Bool "conditional"
                    ) exprs
      ; let b = f (\(VA_Bool b) -> b) bvs
      ; return $ (VA_Bool b)
      }

----------------------
-- HELPER FUNCTIONS --
----------------------

-- http://www.haskell.org/pipermail/haskell-cafe/2003-June/004484.html
powerset       :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = xss /\/ map (x:) xss
    where xss = powerset xs
          (/\/)        :: [a] -> [a] -> [a]
          []     /\/ ys = ys
          (x:xs) /\/ ys = x : (ys /\/ xs)

--------------------
-- ERROR HANDLING --
--------------------

data EvalError = Default String -- FIXME, do I really need this?
               | NoRuleError AS_Expression -- FIXME for debugging only
               | UnknownOperatorError String -- FIXME for debugging only
               | TypeMissmatch Infix_Info VA_Value VA_Value [TY_Type]
               | IllegalType AS_Expression AS_Expression VA_Value
                             TY_Type String
               | NameNotInScope AS_Expression String
               | NameAlreadyBound AS_Expression VA_Value String
               | ValueOutOfBounds AS_Expression AS_Expression AS_Expression
                                  VA_Value Env
               | ChooseNoMatch AS_Expression AS_Expression
                               AS_Expression [VA_Value] Env
               | KeyNotFound Env AS_Expression VA_Value VA_Value String
               | FunAppIllegalOperand AS_Expression VA_Value VA_Value
               | RdBeforeWr AS_Expression
instance Show EvalError where show = ppError

ppError :: EvalError -> String
ppError (Default s) = pp $ text s
ppError (NoRuleError e) =
    -- let pE = parentE e
    --     pU = parentU e
    --     pEdoc = case pE of
    --               Just pE' ->
    --                   text "in expression at " <+>
    --                   text (ppLocE pE') <//> text ":" <$> ppE pE'
    --               Nothing -> empty in
    pp $ nest 4 $ (text $ ppLocE e) <//> text ":" <$>
           (text "No rule to evaluate" <+> parens (ppE e))
ppError (UnknownOperatorError s) = s
ppError (TypeMissmatch (_env, pE, ea, eb) va vb expectedTypes) =
    pp $ nest 4 $ (text $ ppLocE pE) <//> text ":" <$>
           nest 4 (text "Type missmatch. Expected types" <+>
                   (cat (punctuate comma $ map (\ty -> text $ ppTY ty)
                                               expectedTypes)) <//>
                   text ", but found" <+>
                   text (ppTY $ typeOf va) <//> text ", and" <+>
                   text (ppTY $ typeOf vb) <+>
                   text "expressions"
            <$> arm ea va <//> text ", and"
            <$> arm eb vb)
            <$> nest 4 (text "in expression:" <$> ppE pE)
    where arm e v = ppE e <+> parens (text "=" <+> ppVA v) <+>
                    text "at" <+> (text $ ppLocE e)
ppError (IllegalType parentE condE condV expectedType detail) =
    pp $ nest 4 $ (text $ ppLocE condE) <//> text ":" <$>
           nest 4 (text ("Illegal type in " ++ detail)
                   <$> ppE condE <+> parens (text "=" <+> ppVA condV))
           <$> text "expected type" <+> text (ppTY expectedType)
           <//> text ", found type" <+> text (ppTY $ typeOf condV) <+>
                text "in expression"
           <$> indent 4 (ppE parentE)
           <$> text "at" <+> (text $ ppLocE parentE)
ppError (NameNotInScope ident kind) =
    pp $ nest 4 $ (text $ ppLocE ident) <//> text ":"
           <$> text kind <+> dquotes (ppE ident) <+>
               text "not in scope."
ppError (NameAlreadyBound ident v kind) =
    pp $ nest 4 $ (text $ ppLocE ident) <//> text ":"
           <$> text kind <+> dquotes (ppE ident) <+>
               text "is alreaady bound. TLA+ does not allow shadowing."
           <$> indent 4 (ppVA v)
ppError (ValueOutOfBounds i q fappl val env) =
    pp $ nest 4 $ (text $ ppLocE i) <//> text ":"
           <$> text "value of" <+> parens (ppE i) <+>
               text "violated range" <+> ppE q
           <$> text "in expression" <+> ppE fappl <+>
               text "at:" <+> (text $ ppLocE fappl)
           <$> nest 4 (text "where" <+> parens (ppE i) <+> text "was bound to"
                       <$> ppVA val)
           <$> nest 4 (text "in context" -- FIXME only show free vars in q
                       <$> vcat (map (\(id, v) ->
                                   (ppId id) <+> text "==>" <+> ppVA v) env))
ppError (ChooseNoMatch i q expr values env) =
    pp $ nest 4 $ (text $ ppLocE i) <//> text ":"
           <$> text "no value of" <+> parens (ppE i) <+>
               text "in" <+> ppE q
           <$> text "satisfied expression" <+> parens (ppE expr)
           <$> nest 4 (text "tried the following values"
                       <$> vcat (map ppVA values))
           <$> nest 4 (text "in context" -- FIXME only show free vars in expr
                       <$> vcat (map (\(id, v) ->
                                   (ppId id) <+> text "==>" <+> ppVA v) env))
ppError (KeyNotFound _env e idx map kind) =
    pp $ nest 4 $ (text $ ppLocE e) <//> text ":"
           <$> text "key" <+> parens (ppVA idx) <+>
               text "not found in" <+> text kind
           <$> indent 4 (ppVA map)
           <$> text "in expression" <+> parens (ppE e)
ppError (FunAppIllegalOperand e va argv) =
    pp $ nest 4 $ (text $ ppLocE e) <//> text ":"
           <$> nest 4 (text "Expression (function application)" <$> ppVA argv)
           <$> nest 4 (text "cannot be applied to operand" <+>
                       parens (ppVA va)) <$>
               text "of type" <+> text (ppTY $ typeOf va)
ppError (RdBeforeWr e@(AS_Ident _info _quallist name)) =
    pp $ nest 4 $ (text $ ppLocE e) <//> text ":"
           <$> text "read of uninitialized variable" <+> text name
ppError _ = error "unspecified"
-- instance Error EvalError where
--     noMsg = Default "An error has occured"
--     strMsg = Default

type ThrowsError = Either EvalError

pp :: Doc -> String
pp d = showWidth 79 $ d
  where showWidth :: Int -> Doc -> String
        showWidth w doc = displayS (renderPretty 0.9 w doc) ""

trapError action = catchError action (return . show)

extractValue :: HasCallStack => ThrowsError a -> a
extractValue (Right val) = val
extractValue (Left msg) = error $ show msg

------------------
-- ENV HANDLING --
------------------
type Env = [Binding] -- one env per module

type Id = ([String], String)
type Binding = (Id, VA_Value) -- add module qualifiers

mkEmptyEnv = []

bind :: Env -> (AS_Expression, VA_Value) -> ThrowsError Env
bind env (name, expr) = return $ (toId name, expr) : env

replace :: Env -> (AS_Expression, VA_Value) -> ThrowsError Env
replace env (name, expr) =
  let env' = map (\(n,e) -> if n == toId name then (n, expr) else (n, e)) env
   in return env'

lookupBinding :: AS_Expression -> Env -> String -> ThrowsError VA_Value
lookupBinding i env kind =
    case lookup (toId i) env of
      Just expr -> return $ expr
      Nothing -> throwError $ NameNotInScope i kind

toId :: AS_Expression -> Id
toId (AS_Ident _info qual name) = (qual, name)
toId _ = error "unspecified"

ppId :: Id -> Doc
ppId (quallist, name) =
    let l = map text $ quallist++[name] in cat $ punctuate (text "!") l

ppEnv :: Env -> String
ppEnv env =
  pp $ nest 4 (text "Environment"
         <$> vcat (map (\(id, v) ->
                   (ppId id) <+> text "==>" <+> ppVA v) env))

addBuiltIn :: Env -> ThrowsError Env
addBuiltIn e =
    bind e (bif "TLA+" "BOOLEAN" []) >>= \e ->
    bind e (bif "FiniteSet" "Cardinality" ["S"]) >>= \e ->
    bind e (bif "Sequences" "Seq" ["S"]) >>= \e ->
    bind e (bif "TLC" "Assert" ["val", "out"]) >>= \e ->
    bind e (bif "TLC" "Print" ["out", "val"]) >>= \e ->
    bind e (bif "SPECIFICA" "TypeOf" ["val"])
  where
    bif mod name args = (mkIdent name,
                         VA_OperatorDef mkInfoE
                           (AS_OpHead (mkIdent name) (map mkIdent args))
                           (AS_BIF mod name))

-- evalE uses this table for BIFs, the BIF itself does not carry the function
-- because then Syntax would have a
bif_Table = -- merge with addBuiltIn, I hate to update 2 places!
    [(("TLA+", "BOOLEAN"), bif_BOOLEAN)
    ,(("FiniteSet", "Cardinality"), bif_Cardinality)
    ,(("Sequences", "Seq"),         bif_Seq)
    ,(("TLC",       "Assert"),      bif_Assert)
    ,(("TLC",       "Print"),       bif_Print)
    ,(("SPECIFICA", "TypeOf"),      bif_TypeOf)]

mkIdent s = AS_Ident mkInfoE [] s
mkInfoE = mkDummyInfo  "bif-no-location"

bif_BOOLEAN :: Env -> ThrowsError VA_Value
bif_BOOLEAN _env =
    return $ VA_Set $ Set.fromList [VA_Bool True, VA_Bool False]

bif_Cardinality :: Env -> ThrowsError VA_Value
bif_Cardinality env =
    bifArg env "S" >>= \v -> case v of
      (VA_Set s) -> return $ VA_Int $ Set.size s
      other  -> throwError $
        Default ("BIF Cardinality - wrong type of argument "++show other)

bif_Assert :: Env -> ThrowsError VA_Value
bif_Assert env =
    bifArg env "val" >>= \v -> case v of
      (VA_Bool b) ->
          if b
            then return $ VA_Bool True
            else let o = bifArg env "out"
                  in throwError $ Default ("Assertion violation "++(show o))
      other  -> throwError $
        Default ("BIF Assert - wrong type of argument "++show other)

bif_Print :: Env -> ThrowsError VA_Value
bif_Print env =
    bifArg env "val" >>= \v ->
      bifArg env "out" >>= \o ->
        Trace.trace (prettyPrintVA o) $ return v

bif_TypeOf :: Env -> ThrowsError VA_Value
bif_TypeOf env =
    bifArg env "val" >>= \v ->
        return $ VA_String $ ppTY (typeOf v)

bif_Seq :: Env -> ThrowsError VA_Value
bif_Seq env = bifArg env "S" >>= \v ->
                return $ VA_SeqType v

bifArg :: Env -> String -> ThrowsError VA_Value
bifArg env arg =
    lookupBinding (mkIdent arg) env ("BIF paramter "++arg)
