module GenSLA(genSLAFile) where

import Data.List(nub, isPrefixOf)

import Flatten
import Data.Generics
import Syntax
import RewriteTimer(allRoles)
import TLACodeGen(typeKernel, subst, Pattern)
import ParserHelper(inlineOperatorDef)

import Data.List((\\))

import Text.ParserCombinators.Parsec.Pos as PPos

import Language.TLAPlus.Pretty(ppUnit)
import Language.TLAPlus.Syntax as TLASyntax

import Text.PrettyPrint.Leijen -- FIXME kramer@acm.org reto -- hack

genSLAFile :: String -> SH_FL_Spec -> AS_Spec -> IO ()
genSLAFile pname spec tla =
  let roles = allRoles spec \\ ["GLOBAL"]
      s1 = "---- MODULE replication ----\n\
\ CONSTANT " ++ commaSep roles ++ "\n\
\ \n\
\ fontsize == \"tiny\"\n\
\ \n\
\ d(seqA, seqB) == \\* (seqA \\ seqB) for sequences (def over set of elements)\n\
\   LET range(seq) == { seq[i] : i \\in DOMAIN(seq) }\n\
\    IN range(seqA) \\ range(seqB)\n\
\ \n\
\ mc(set) == { <<s, {}>> : s \\in set }\n\
\ \n\
\ nonEmpty(set) == { s \\in set: s[2] # {} } \n\
\ \n\
\ singleton(s) == CHOOSE x \\in s: TRUE\n\
\ \n\
\ remIfIn2ndA(multi, single) ==\n\
\   IF single = {} \n\
\   THEN multi\n\
\   ELSE IF multi = {} \n\
\        THEN multi\n\
\        ELSE LET multi2 == singleton(multi)\n\
\              IN {<<multi2[1], \n\
\                    { m \\in multi2[2] : \n\
\                        ~ \\E s \\in single: m[1] = singleton(s[2])[1] }>>}\n\
\ \n\
\ remIfIn2ndB(single, multi) == \n\
\   IF single = {} \n\
\   THEN single\n\
\   ELSE IF multi = {} \n\
\        THEN single\n\
\        ELSE LET multi2 == singleton(multi)\n\
\              IN IF \\E m \\in multi2[2]: \n\
\                       singleton(singleton(single)[2])[1] = m[1]\n\
\                 THEN {}\n\
\                 ELSE single\n\
\ \n\
\ remMCDest(set) ==\n\
\   LET card(set) == IF Cardinality(set) = 0 \n\
\                    THEN 1 \n\
\                    ELSE Cardinality(set)\n\
\       strip(s) ==\n\
\         IF s[2] = {}\n\
\         THEN s\n\
\         ELSE LET ms == { <<m[1], card(m[2]), m[2]>> : m \\in s[2] } \n\
\               IN <<s[1], ms, s[3]>>\n\
\    IN { strip(s) : s \\in set} \n\
\ \n\
\ (* code gen !!! *)\n\
\ sent(states, pair) ==\n\
\   LET g_obuf_add == nonEmpty(\n" ++
             (cup "          {<<pair, d(states[pair.next].state.st_%r%[a].g_obuf,\n\
\                     states[pair.curr].state.st_%r%[a].g_obuf), a\n\
\         >> : a \\in %r% }\n" (sendMC spec)) ++ ")" ++
           "       g_obuf_rem == nonEmpty(\n" ++
             (cup "          {<<pair, d(states[pair.curr].state.st_%r%[a].g_obuf,\n\
\                     states[pair.next].state.st_%r%[a].g_obuf), a\n\
\         >> : a \\in %r% }\n" (sendMC spec)) ++ ")" ++
           "       inbox_add == nonEmpty(\n" ++
             (cup "                {<<pair, mc(d(states[pair.next].state.st_%r%[a].g_inbox,\n\
\                             states[pair.curr].state.st_%r%[a].g_inbox)), a\n\
\              >> : a \\in %r% }\n" roles) ++ ")" ++
           "    IN remMCDest(       remIfIn2ndA(g_obuf_add, inbox_add)\n\
\                  \\cup remIfIn2ndB(inbox_add, g_obuf_rem))\n\
\ \n\
\ recv(states, pair) ==\n" ++
           "   LET inbox_rem == nonEmpty(\n" ++
             (cup "                {<<pair, d(states[pair.curr].state.st_%r%[a].g_inbox,\n\
\                             states[pair.next].state.st_%r%[a].g_inbox), a\n\
\              >> : a \\in %r% }\n" roles) ++ ")" ++
           "    IN inbox_rem\n\
\ \n\
\ flattenS(set) ==\n\
\   UNION { { << e, s[1].curr, e[1].sender, s[3] >>: \n\
\           e \\in s[2] } : s \\in set }\n\
\ \n\
\ flattenR(set) ==\n\
\   UNION { { << e, s[1].curr, s[3] >>: \n\
\           e \\in s[2] } : s \\in set }\n\
\ \n\
\ nLowest(card, set) ==\n\
\   LET x == IF \\/ Cardinality(set) > card\n\
\            THEN CHOOSE x \\in SUBSET set \\ {{}}: \n\
\                   /\\ Cardinality(x) = card\n\
\                   /\\ \\A s \\in x: \\E y \\in set \\ x: y[2] > s[2]\n\
\            ELSE set\n\
\    IN x\n\
\ \n\
\ smallestMatch(src, targets) ==\n\
\   LET msg == src[1][1]\n\
\       card == src[1][2]\n\
\       srcState == src[2]\n\
\       srcAgent == src[3]\n\
\       tarAgent == src[4]\n\
\       ts == { IF /\\ t[1] = msg  \n\
\                  /\\ ((t[2] > srcState) \\/ (t[2] = srcState))\n\
\                  /\\ (t[3] = tarAgent \\/ t[3] \\in src[1][3])\n\
\               THEN { t }\n\
\               ELSE {} : t \\in targets }\n\
\       x == UNION ts\n\
\       y == nLowest(card, x)\n\
\    IN y\n\
\ \n\
\ msgPairs(sSet,rSet) ==\n\
\   UNION { mkMsgRec(s, smallestMatch(s, rSet)) : s \\in sSet }\n\
\ \n"

      s2 = "mkMsgRec(s, rSet) ==\n\
\   LET msg == s[1][1]\n\
\       mkMsg(s,r) == [label |-> msg, tiplabel |-> msg.type,\n\
\                      font  |-> fontsize, tipfont |-> fontsize, \n\
\                      color |-> \"@@color@@\", style |-> \"@@style@@\",\n\
\                      from  |-> [agent |-> s[3], state |-> s[2]], \n\
\                      to    |-> [agent |-> r[3], state |-> r[2]]]\n\
\    IN { mkMsg(s,r) : r \\in rSet}\n"

      s3 = " extract_msgs[states \\in {}] == \\* FIXME enforce format, or use don't care\n\
\   LET pairs(n) == { [curr |-> i, next |-> i+1] : i \\in 1 .. n-1 }\n\
\       ps == pairs(Cardinality(DOMAIN states))\n\
\       s == nonEmpty(UNION { sent(states, pair) : pair \\in ps })\n\
\       r == nonEmpty(UNION { recv(states, pair) : pair \\in ps })\n\
\       mps == msgPairs(flattenS(s), flattenR(r))\n\
\    IN mps\n\
\ \n\
\ sf[transition \\in {}] ==\n\
\   [label    |-> transition.diff,\n\
\    hidediff |-> " ++
           "{}, \\* {" ++ commaSep (map show $ generatedStateFields tla) ++ "},\n" ++
           "    font     |-> fontsize]\n\
\ \n\
\ ASSUME [ messages |-> extract_msgs, state_transitions |-> sf]\n\
\ ====\n"
      ce = extractColorAnn spec
      se = extractStyleAnn spec
      s2' = " " ++ (prettyPrintUnit (rewriteAnn ce se (inlineOperatorDef s2)))
      s = s1 ++ s2' ++ "\n\n" ++ s3
  in writeFile (pname ++ ".sla") s

seperate :: String -> [String] -> String
seperate _ [] = ""
seperate x (a:[]) = a
seperate x (a:b) = a ++ x ++ seperate x b

commaSep :: [String] -> String
commaSep l = seperate ", " l

cupSep :: [String] -> String
cupSep l = seperate " \\cup " l

sendMC :: SH_FL_Spec -> [String] -- list of roles that send MC
sendMC spec = nub $ concat $ map xtract $ msgDecl spec
  where xtract (SH_MsgDecl _ s r _ _) | isTypeSet r = typeKernel s
                                      | otherwise   = []

recvMC :: SH_FL_Spec -> [String] -- list of roles that recv MC
recvMC spec = nub $ concat $ map xtract $ msgDecl spec
  where xtract (SH_MsgDecl _ _s r _ _) | isTypeSet r = typeKernel r
                                       | otherwise   = []

cup :: String -> [String] -> String
cup template roles =
  let fragments = map (instTemplate template) roles
   in cupSep fragments
  where instTemplate t role = subst [("%r%", role)] t

generatedStateFields :: AS_Spec -> [String]
generatedStateFields tla = nub $ everything (++) ([] `mkQ` f) tla
  where f (AS_Ident _ _ name) | isPrefixOf "g_" name = [name]
                              | otherwise        = []
        f _ = []

-- FIXME kramer@acm.org reto -- hack
prettyPrintUnit :: AS_UnitDef -> String
prettyPrintUnit u = showWidth 79 $ ppUnit u
    where showWidth :: Int -> Doc -> String
          showWidth w doc = displayS (renderPretty 0.9 w doc) ""

rewriteAnn :: AS_Expression -> AS_Expression -> AS_UnitDef -> AS_UnitDef
rewriteAnn colorExpr styleExpr
           (AS_OperatorDef info h@(AS_OpHead (AS_Ident _ _ "mkMsgRec") _) e) =
    let e' = everywhere (mkT (f (colorExpr,styleExpr))) e
     in AS_OperatorDef info h e'
  where f :: (AS_Expression, AS_Expression) -> AS_Expression -> AS_Expression
        f (ce,se) (AS_StringLiteral _ "@@color@@") = ce
        f (ce,se) (AS_StringLiteral _ "@@style@@") = se
        f _ x = x
rewriteAnn _ _ x = x

-- FIXME kramer@acm.org reto -- code duplication
extractColorAnn spec =
  let allMsgAnn = filter isMsgAnn $ extractAllDisplaySLs spec
      allColor = concat $ map colorEntry allMsgAnn
      arms = map (\(mtype, (SH_ExprWrapper _ e)) ->
                     AS_CaseArm epos
                       (AS_InfixOP epos AS_EQ
                        (AS_InfixOP epos AS_DOT
                         (mk_AS_Ident "msg")
                         (mk_AS_Ident "type"))
                        (mk_AS_Ident $ show mtype))
                       (rewriteNames mtype spec e)
                 )
             allColor
      otherArm = AS_OtherCaseArm epos $ AS_StringLiteral epos defaultColor
   in AS_Case epos arms (Just otherArm)
  where isColor SH_SL_MsgAnnColor = True
        isColor _ = False
        colorEntry (SH_SL_MsgAnn mtype l) =
            concat $ map (\(k,e) -> if isColor k then [(mtype, e)] else []) l

-- FIXME kramer@acm.org reto -- code duplication
extractStyleAnn spec =
  let allMsgAnn = filter isMsgAnn $ extractAllDisplaySLs spec
      allStyle = concat $ map colorEntry allMsgAnn
      arms = map (\(mtype, (SH_ExprWrapper _ e)) ->
                     AS_CaseArm epos
                       (AS_InfixOP epos AS_EQ
                        (AS_InfixOP epos AS_DOT
                         (mk_AS_Ident "msg")
                         (mk_AS_Ident "type"))
                        (mk_AS_Ident $ show mtype))
                       (rewriteNames mtype spec e)
                 )
             allStyle
      otherArm = AS_OtherCaseArm epos $ AS_StringLiteral epos defaultStyle
   in AS_Case epos arms (Just otherArm)
  where isStyle SH_SL_MsgAnnStyle = True
        isStyle _ = False
        colorEntry (SH_SL_MsgAnn mtype l) =
            concat $ map (\(k,e) -> if isStyle k then [(mtype, e)] else []) l

isMsgAnn (SH_SL_MsgAnn _ _) = True

extractAllDisplaySLs :: SH_FL_Spec -> [SH_SL_Ann]
extractAllDisplaySLs spec = (everything (++) ([] `mkQ` f)) spec
  where f (SH_DisplaySwimlane _ l) = l
        f _ = []

rewriteNames :: String -> SH_FL_Spec -> AS_Expression -> AS_Expression
rewriteNames mtype spec e = everywhere (mkT (f mtype spec)) e
  where f mtype spec i@(AS_Ident _ _ s) =
            let fields = allFieldsOfMsg mtype (msgDecl spec)
             in if elem s fields
                then AS_InfixOP epos AS_DOT  -- protect field
                         (mk_AS_Ident "msg")
                         i
                else AS_StringLiteral epos s -- assume it's color or style name
        f _ _ x = x

allFieldsOfMsg :: String -> [SH_MsgDecl] -> [String]
allFieldsOfMsg mtype l = concat $ map (allFieldsOfMsg0 mtype) l
  where allFieldsOfMsg0 :: String -> SH_MsgDecl -> [String]
        allFieldsOfMsg0 mtype (SH_MsgDecl _ _ _ t l)
            | t == mtype = fieldNames l
            | otherwise = []
        fieldNames l = let (_, names) = unzip l in names

defaultColor = "black"
defaultStyle = "solid"

-----
mk_AS_Ident s = AS_Ident epos [] s

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos name line col = newPos name line col

upos = mkPos "foo" 0 0
epos = (upos, Nothing, Nothing)
