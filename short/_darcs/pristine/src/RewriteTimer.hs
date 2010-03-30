module RewriteTimer where

import Char (toLower)
import Data.Generics
import Syntax
import Flatten
import ParserHelper(inlineOperatorDef)
import TLACodeGen(mk_AS_Type, combineInfix)

import Text.ParserCombinators.Parsec.Pos as PPos
import Language.TLAPlus.Syntax as TLASyntax

rewriteTimer :: SH_FL_Spec -> SH_FL_Spec
rewriteTimer spec = 
  let timeline_init = genTimelineInit spec
      everyLess = rewriteEvery spec
   in if noTimers everyLess 
      then   rewriteTimer0 
	   $ everyLess
      else   rewriteLinearStutter
           $ rewriteTimer0 
           $ rewriteTimerBoilerplate timeline_init 
           $ everyLess
  where noTimers spec =
	    [] == (concat $ map (\r -> allTimers spec r) (allRoles spec))

dummyI = "dummyInteraction"

rewriteTimerBoilerplate :: AS_Expression -> SH_FL_Spec -> SH_FL_Spec
rewriteTimerBoilerplate timeline_init spec =
    let d0 = SH_VerbTLAOp upos dummyI Nothing 
	       (AS_OperatorDef upos 
		  (AS_OpHead (mk_AS_Ident "Timers") []) 
	          (mk_AS_Type -- TLACodeGen, makes it conventient to express ty
		     (SH_Ty_Union upos $
		       concat $ map (\r ->
		                 let t = allTimers spec r 
				  in if t == [] 
				     then [] -- no pair
				     else [SH_Ty_PairOf upos 
					    (SH_Ty_UserDef upos r)
					    (SH_Ty_Enum upos t)]) 
		                 (allRoles spec))))
	s = SH_State upos False
	      (SH_Ty_SetOf upos 
	       (SH_Ty_PairOf upos 
		(SH_Ty_UserDef upos "Time")
		(SH_Ty_UserDef upos "Timers")),
	       "g_timeline")
	      (Just $ SH_ExprWrapper upos timeline_init)
	t = SH_CallHandler upos "global" Nothing "Tick" [] 
	      Nothing -- FIXME kramer@acm.org reto, consider well known hook  
	      (genTick spec)
        g = SH_RoleDef upos "GLOBAL" [] [s, t]
	b0 = SH_VerbTLAOp upos dummyI Nothing genTimeType
	b1 = SH_VerbTLAOp upos dummyI Nothing genNow
	b2 = SH_VerbTLAOp upos dummyI Nothing genNowOp
	b3 = SH_VerbTLAOp upos dummyI Nothing genTimeMax
	a0 = SH_VerbTLAOp upos dummyI Nothing genLinearTimeMaxd
	a = SH_VerbTLAOp upos dummyI Nothing genTimerRemoveAndSchedule
	b = SH_VerbTLAOp upos dummyI Nothing genTimerNext
	c = SH_VerbTLAOp upos dummyI Nothing genRoleNext
     in spec { roleDecl = if hasGlobalRole spec -- create global role if it
	                  then roleDecl spec    -- does not already exist
	                  else g : (roleDecl spec),
	       -- b1,2 are NOW related - may want those in user defined
	       -- PROPERTIES, hence they need to come before the user def
	       -- ones.
	       verbTLA  = b1 : b2: b3: verbTLA spec ++ [d0, b0, a0, a, b, c] }
  where genTick spec = 
	    let roles = allRoles spec
	     in (concat $ map (genTickBranch spec) roles) 
                 ++ [SH_GuardedInstrList upos 
		     (Just $ SH_ExprWrapper upos (mk_AS_Ident "OTHER")) 
		     Nothing 
		     [SH_I_FailTLAClause upos]] -- [] OTHER -> FALSE
        genTickBranch spec role =
	    let timers = allTimers spec role
	     in map ( \ timer ->
                      SH_GuardedInstrList upos 
		        (Just $ SH_ExprWrapper upos 
		           (AS_InfixOP epos AS_EQ
		             (AS_OpApp epos (mk_AS_Ident "TimerNext") 
		                [AS_InfixOP epos AS_DOT 
				 -- FIXME kramer@acm.org reto -- hardcoded st_
				   (mk_AS_Ident "st_GLOBAL")
				   (mk_AS_Ident "g_timeline")])
			     (AS_StringLiteral epos timer)))
		        Nothing 
		        [SH_I_ForeignChangeState upos role 
			  (Just $ AS_OpApp epos (mk_AS_Ident "RoleNext") 
		                     [AS_InfixOP epos AS_DOT 
				 -- FIXME kramer@acm.org reto -- hardcoded st_
				        (mk_AS_Ident "st_GLOBAL")
				        (mk_AS_Ident "g_timeline")])
			  [SH_ExprWrapper upos 
			     (AS_InfixOP epos AS_EQ 
			       (mk_AS_Ident $ enable timer)
			       (AS_Bool epos True))]])
                    timers

-- FIXME kramer@acm.org reto -- structure this better, remove code dup.
-- only the TimeoutHandler needs rewriting into a CallHandler, we do the same
-- thing to all the other Handlers however, so all we need is a way to 
-- determine whether something's a handler. 
rewriteTimer0 :: SH_FL_Spec -> SH_FL_Spec
rewriteTimer0 = everywhere (mkT f)
    where f (SH_Timer _ _ name) = 
	      SH_State upos False 
		(SH_Ty_UserDef upos "BOOLEAN", enable name)
		(Just (SH_ExprWrapper upos (mk_AS_Ident "FALSE")))
	  f (SH_MsgHandler _ ann role when mtype label any from ginstr) =
	      SH_MsgHandler upos ann role when mtype label any from
		(appendInstr role Nothing ginstr)
	  f (SH_CallHandler _ role when label args hook ginstr) =
	      SH_CallHandler upos role when label args hook
		(appendInstr role Nothing ginstr)
	  f (SH_TimeoutHandler _ role when name hook ginstr) =
	      SH_CallHandler upos -- NOTE timeout turns into a call handler
	        role (conj when (enable(name))) (timeout name) [] hook
	        (appendInstr role (Just name) ginstr)
	  f (SH_CrashHandler _ ann role when remoteRole id hook ginstr) =
	      SH_CrashHandler upos ann role when remoteRole id hook
		(appendInstr role Nothing ginstr)
	  f (SH_Every _ role guard period hook ginstr) =
	      SH_Every upos role guard period hook
		(appendInstr role Nothing ginstr)
	  f (SH_Extend_Hook _ role hook ginstr) =
	      SH_Extend_Hook upos role hook
		(appendInstr role Nothing ginstr)
	  f x = x

conj :: (Maybe SH_ExprWrapper) -> String -> (Maybe SH_ExprWrapper)
conj Nothing e = 
    Just (SH_ExprWrapper upos (mk_AS_Ident e))
conj (Just (SH_ExprWrapper _ a)) e = 
    Just (SH_ExprWrapper upos (AS_LAND epos [a, mk_AS_Ident e]))

updateSched :: String -> (Maybe String) -> [(AS_Expression, String)] 
	    -> [String] -> SH_Instr
updateSched role name restarted canceled =
    let b = AS_OpApp epos 
	        (mk_AS_Ident "TimerRemoveAndSchedule")
		[AS_InfixOP epos AS_DOT 
		   -- FIXME kramer@acm.org reto -- hardcoded st_
		   (mk_AS_Ident "st_GLOBAL")
		   (mk_AS_Ident "g_timeline"),
		 case name of -- remove current timer that just fired
		   Nothing -> AS_DiscreteSet epos []
		   Just name ->
		     AS_DiscreteSet epos -- {} or singleton
		       [AS_Tuple epos 
			[mk_AS_Ident role,
			 AS_StringLiteral epos name]],
		 AS_DiscreteSet epos -- restarted timers
		    (map ( \ (delta_expr, tname) ->
		             AS_Tuple epos [
			       delta_expr,
			       AS_Tuple epos [
			         mk_AS_Ident role,
				 AS_StringLiteral epos tname]]) 
		         restarted),
		 AS_DiscreteSet epos -- canceled timers
		    (map ( \ tname -> AS_Tuple epos [
				        mk_AS_Ident role,
					AS_StringLiteral epos tname]) 
		         canceled)]
	a = SH_ExprWrapper upos $
	      AS_InfixOP epos AS_EQ (mk_AS_Ident "g_timeline") b
     in SH_I_ForeignChangeState upos "GLOBAL" Nothing [a]

restartedT :: [SH_Instr] -> [(AS_Expression, String)]
restartedT l = concat $ map restartedT0 l
  where restartedT0 (SH_I_Timerrestart _ (SH_ExprWrapper _ e) name) = 
	    [(e, name)]
	restartedT0 _ = []

canceledT :: [SH_Instr] -> [String]
canceledT l = concat $ map canceledT0 l
  where canceledT0 (SH_I_Timercancel _ name) = [name]
	canceledT0 _ = []

-- Because we rewrite the timeout handler _after_ the global tick call handler
-- is created (since that creation needs to SH_Timer intact (which rewrite0
-- remove by turning it into a call handler), we are forced to explude the
-- global tick handler from the rewrites done to call handlers in all other
-- roles.  
appendInstr :: String -> (Maybe String) -> [SH_GuardedInstrList] 
	    -> [SH_GuardedInstrList]
appendInstr "global" _ l = l 
appendInstr role name l = map (appendInstr0 role name) l 
appendInstr0 :: String -> (Maybe String) -> SH_GuardedInstrList 
	     -> SH_GuardedInstrList
appendInstr0 role name ginstr =
    let SH_GuardedInstrList info guard label l = ginstr
	instr = let r = restartedT l
		    -- NOTE: TIMERRESTART will implicitly CANCEL any already 
		    -- scheduled timer of the same name (++ map , below)
		    c = (canceledT l) ++ -- by way of a CANCEL instruction
			(map (\(_,n) -> n) r) -- extract timername for cancel
	         in if (name == Nothing) && (r == []) && (c == [])
		    then []
		    else let Just n = name
			  in [updateSched role name r c]
	disable = case name of
		    Nothing -> []
		    Just n -> [SH_I_ChangeState upos 
			        [SH_ExprWrapper upos 
				   (AS_InfixOP epos AS_EQ 
				     (mk_AS_Ident $ enable n)
				     (mk_AS_Ident "FALSE"))]]
     in SH_GuardedInstrList info guard label (l ++ instr ++ disable)

rewriteEvery :: SH_FL_Spec -> SH_FL_Spec
rewriteEvery spec = everywhere (mkT f) spec
    where f roledef@(SH_RoleDef _ rname vars elems) =
	      let genTimers = map (\(SH_Every _ _ _ period _ ginstr) -> 
			              SH_Timer upos rname $ every period) 
			        (allEvery elems)
		  new_elems = concat $ map (replEvery rname) elems
	      in SH_RoleDef upos rname vars $ new_elems ++ genTimers
	  f x = x
          allEvery :: [SH_RoleElement] -> [SH_RoleElement]
	  allEvery l = filter (\e -> case e of
			               (SH_Every _ _ _ _ _ _) -> True
			               _ -> False) l
	  -- No guard case
	  replEvery (rname) (SH_Every _ role Nothing period hook ginstr) =
	      let i = SH_I_Timerrestart upos period (every period)
	       in [SH_TimeoutHandler upos (lower rname) Nothing 
		      (every period) hook (appendInstrRaw i ginstr)]
	  -- Guarded every case, here we need to make sure that the first
	  -- handler invocation when the guard became false will in fact
	  -- remove the timeline entry and stop the handler from being
	  -- rescheduled. If the every statement is guarded, we thus create
	  -- two handlers to cover the regular and the case where the guard 
	  -- is false to disable (i.e. not restart) the timer.	  
	  replEvery (rname) (SH_Every _ role guard period hook ginstr) =
	      let i = SH_I_Timerrestart upos period (every period)
		  j = SH_I_Timercancel upos (every period)
	       in [SH_TimeoutHandler upos (lower rname) guard -- regular guard
		      (every period) hook (appendInstrRaw i ginstr),
	           -- this is the handler tasked with removing the timer from
		   -- the global schedule
		   SH_TimeoutHandler upos (lower rname) (negate guard) -- neg g
		      (every period) hook 
		      [SH_GuardedInstrList upos Nothing Nothing [j]]]
	  replEvery _ x = [x]
	  negate (Just (SH_ExprWrapper _ e)) = 
 		  Just (SH_ExprWrapper upos (AS_PrefixOP epos AS_Not e))

-- only generate for linear time, not for wrap around mode
rewriteLinearStutter :: SH_FL_Spec -> SH_FL_Spec
rewriteLinearStutter spec =
    if True -- FIXME kramer@acm.org reto -- if time mode is linear only
      then let g = SH_RoleDef upos "GLOBAL" [] []
	       spec' = if hasGlobalRole spec 
		         then spec
		         else spec { roleDecl = g : (roleDecl spec) }
            in everywhere (mkT f) spec'
      else spec
  where f (SH_RoleDef _ "GLOBAL" vars l) =
	    let guard = AS_OpApp epos
			  (mk_AS_Ident "LinearTimeMaxd") 
			  [AS_InfixOP epos AS_DOT 
		             -- FIXME kramer@acm.org reto -- hardcoded st_
			     (mk_AS_Ident "st_GLOBAL")
			     (mk_AS_Ident "g_timeline")]
		stutter = SH_CallHandler upos "global" 
			    (Just $ SH_ExprWrapper upos guard) 
			    "StutterWhenLinearTimeMaxd" [] Nothing []
		-- FIXME kramer@acm.org reto -- the global state has to have
		-- some state since otherwise, there's UNCHANGED<st_GLOBAL>
	        -- references in the "Stutter..." action, but no st_GLOBAL
		-- will have been generated.
		fakeState = SH_State upos False 
			      (SH_Ty_UserDef upos "BOOLEAN",
			       "g_dummy2")
			      (Just $ SH_ExprWrapper upos (AS_Bool epos False))
             in SH_RoleDef upos "GLOBAL" vars (l ++ [stutter] ++ [fakeState])
        f x = x

appendInstrRaw :: SH_Instr -> [SH_GuardedInstrList] -> [SH_GuardedInstrList] 
appendInstrRaw instr l = map (appendInstrRaw0 instr) l

appendInstrRaw0 :: SH_Instr -> SH_GuardedInstrList -> SH_GuardedInstrList
appendInstrRaw0 instr ginstr =
    let SH_GuardedInstrList info guard label l = ginstr
     in SH_GuardedInstrList info guard label (l ++ [instr])

allTimers :: SH_FL_Spec -> String -> [String]
allTimers spec role = (everything (++) ([] `mkQ` (f role))) spec
  where f role t@(SH_Timer _ r name) | r == role = [name]
	f _ _ = []

genTimelineInit :: SH_FL_Spec -> AS_Expression -- union of sets
genTimelineInit spec =
    let roles = allRoles spec
	l = map (f spec) roles
     in if concat l == [] 
	then AS_DiscreteSet epos [] -- in case there's no EVERY
	else combineInfix AS_Cup (concat l) -- EVERY present will lead here
  where f :: SH_FL_Spec -> String -> [AS_Expression]
        f spec r = let periods = allEveryPeriodsForRole spec r
		    in map (\p -> AS_SetGeneration epos 
			           (AS_Tuple epos [
				      AS_Num epos 0, 
				      AS_Tuple epos [mk_AS_Ident (lower r),
						     AS_StringLiteral epos p]])
			           (AS_QBound1 
				      (mk_AS_Ident (lower r))
				      (mk_AS_Ident r)))
		           (map every periods)

 
allEveryPeriodsForRole :: SH_FL_Spec -> String -> [SH_ExprWrapper]
allEveryPeriodsForRole spec rname =
  let [role] = everything (++) ([] `mkQ` (f rname)) spec
      everyL = everything (++) ([] `mkQ` g) role
      periods = map (\(SH_Every _ _ _ period _ _) -> period) everyL
   in periods
  where f rname r@(SH_RoleDef _ name _ _) | name == rname = [r]
	f _ _ = []
	g e@(SH_Every _ _ _ _ _ _) = [e]
	g _ = []

allRoles :: SH_FL_Spec -> [String]
allRoles = everything (++) ([] `mkQ` f)
  where f (SH_RoleDef _ name _ _) = [name]
	f _ = []

enable s = "g_" ++ s ++ "_enabled"
timeout s = s ++ "_timeout"
-- FIXME kramer@acm.org reto -- really I need a counter env to number the 
-- "every generated" timers.
every (SH_ExprWrapper _ (AS_Ident _ _ s)) = "every_" ++ s
every (SH_ExprWrapper _ (AS_Num _ i)) = "every_" ++ show i

-- NOTE kramer@acm.org reto -- MaxTime can either be a constant in which
-- case the user must declare it as CONSTANT and add it to the .cfg file,
-- or it can be defined as TLA { MaxTime = x * SomeTimerPeriod }
genTimeType = 
    inlineOperatorDef $ unlines 
      (["Time == 0 .. MaxTime"])

genNow =
    inlineOperatorDef $ unlines 
      (["Now(timeline) ==",
	"  IF timeline = {}",
	"    THEN 0",
	"    ELSE LET times == { x[1] : x \\in timeline}",
	"          IN CHOOSE t \\in times: \\A tt \\in times: t <= tt"])

-- FIXME kramer@acm.org reto -- hardcoded st_GLOBAL reference
genNowOp =
    inlineOperatorDef $ unlines 
      (["NOW == Now(st_GLOBAL.g_timeline)"]) 

genTimeMax =
    inlineOperatorDef $ unlines 
      (["Min2(a,b) == IF a < b THEN a ELSE b"]) 

genLinearTimeMaxd =
    inlineOperatorDef $ unlines 
      (["LinearTimeMaxd(timeline) ==",
	"  IF timeline = {}",
	"    THEN FALSE",
	"    ELSE \\E x \\in timeline : x[1] = MaxTime+1"])

-- FIXME kramer@acm.org reto -- For the linear clock, I should really stop 
-- the clock the very first time "now + x[1]" exceeds MaxTime and stutter 
-- from there.
genTimerRemoveAndSchedule = 
    inlineOperatorDef $ unlines 
      (["TimerRemoveAndSchedule(timeline,remove,new,cancel) ==",
	"  LET now == Now(timeline)",
	"      removeX == { t \\in timeline: \\E p \\in remove: t[2] = p } ",
	"      cancelX == { t \\in timeline: \\E p \\in cancel: t[2] = p } ",
	"      rcSet == removeX \\cup cancelX",
	"      ctimeline == timeline \\ rcSet",
	-- never really go over the MaxTime to avoid range errors, however
	-- allow a timer to fire at MaxTime precisely, it's next reschedule
	-- from there will be MaxTime+1 which the getTimer/RoleNext trigger
	-- on to freeze the time.
	"      newSet == {<<Min2(now+x[1], MaxTime+1), x[2]>> : x \\in new }",
	"      newSetPruned == { t \\in newSet : t[1] <= MaxTime+1 }",
	"   IN ctimeline \\cup newSetPruned"])

{-
-- mod MaxTime clock
	"      newSet == { << (now + x[1]) % MaxTime, x[2]>> : x \\in new }",
	"   IN ctimeline \\cup newSet"])
--	"      newSet == { <<now + x[1], x[2]>> : x \\in new }",
--	"      newSetPruned == { t \\in newSet : t[1] <= MaxTime }",
--	"   IN ctimeline \\cup newSetPruned"])
-}

-- no longer return timers if at least one of them hit the MaxTime
-- FIXME kramer@acm.org reto -- only applicable if we generate for linear
--                              time
genTimerNext =
    inlineOperatorDef $ unlines 
      (["TimerNext(timeline) == ",
	"  IF timeline = {} \\/ LinearTimeMaxd(timeline)",
	"  THEN \"\" \\* some value that is not in the Timers set",
	"  ELSE LET now == Now(timeline)",
	"           entry == CHOOSE x \\in timeline: x[1] = now",
	"        IN entry[2][2] \\* timer in <<t, <<role, _timer_>> >>"])

genRoleNext =
    inlineOperatorDef $ unlines 
      (["RoleNext(timeline) ==",
	"  IF timeline = {} \\/ LinearTimeMaxd(timeline)",
	"  THEN \"\" \\* some value that is not in the Timers set",
	"  ELSE LET now == Now(timeline)",
	"           entry == CHOOSE x \\in timeline: x[1] = now",
	"        IN entry[2][1] \\* role in <<t, <<_role_, timer>> >>"])

lower = map toLower

hasGlobalRole :: SH_FL_Spec -> Bool
hasGlobalRole spec = [] /= (everything (++) ([] `mkQ` f)) spec
    where f (SH_RoleDef _ "GLOBAL" _ _) = [True]
	  f _ = []

---- HELPER -------------------------------------------------------------------
mk_AS_Ident s = AS_Ident epos [] s

mkPos :: String -> Int -> Int -> PPos.SourcePos
mkPos name line col = newPos name line col

upos = mkPos "foo" 0 0 
epos = (upos, Nothing, Nothing)
