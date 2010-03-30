---- PARTS ----
protocol prim_startrole concern default {
  roles R,S
  interaction i {
    role R {
      state BOOLEAN xr
       handle crash S s  {
          change xr = TRUE
      }
    }
    role S {
      state BOOLEAN xs
       handle crash R r  {
          change xs = TRUE
      }
    }
  }
}
--------


-------- AFTER Merging interactions ---------------
protocol prim_startrole concern __generated {
  roles R,S
  interaction __generated {
    role R {
      state BOOLEAN xr
       handle crash S s  {
          change xr = TRUE
      }
    }
    role S {
      state BOOLEAN xs
       handle crash R r  {
          change xs = TRUE
      }
    }
  }
}
--------

-------- AFTER insideOut (flatten) ----------------
 




role R {
  state BOOLEAN xr
   handle crash S s  {
      change xr = TRUE
  }
}
role S {
  state BOOLEAN xs
   handle crash R r  {
      change xs = TRUE
  }
}
--------

-------- AFTER rewriteWhen ------------------------
 




role R {
  state BOOLEAN xr
   handle crash S s  {
      change xr = TRUE
  }
}
role S {
  state BOOLEAN xs
   handle crash R r  {
      change xs = TRUE
  }
}
--------

-------- AFTER rewriteTAG -------------------------
 
 tla {   NullSender(msg) ==
           [msg EXCEPT !.sender = 0] }
 tla {   ANY(s) ==
           IF Assert(\A a,b \in s: NullSender(a) = NullSender(b),
                     <<
                     "ANY(s), not all elements (other than 'sender') are equal!",
                     s>>)
           THEN NullSender(CHOOSE e \in s: TRUE)
           ELSE FALSE }
 tla {   ANY2(s,fieldname) ==
           LET ss ==
                 {val[fieldname]: val \in s}
            IN IF Assert(\A a,b \in ss: a = b,
                         <<"ANY2(s,fieldname), not all elements are equal!",
                           s,
                           fieldname,
                           ss>>)
               THEN CHOOSE e \in ss: TRUE
               ELSE FALSE }
 tla {   ALL(s) ==
           s }
 tla {   SENDERS(s) ==
           {m.sender: m \in ALL(s)} }



role R {
  state BOOLEAN xr
   handle crash S s  {
      change xr = TRUE
  }
}
role S {
  state BOOLEAN xs
   handle crash R r  {
      change xs = TRUE
  }
}
--------

-------- AFTER rewriteStateInit -------------------
 
 tla {   NullSender(msg) ==
           [msg EXCEPT !.sender = 0] }
 tla {   ANY(s) ==
           IF Assert(\A a,b \in s: NullSender(a) = NullSender(b),
                     <<
                     "ANY(s), not all elements (other than 'sender') are equal!",
                     s>>)
           THEN NullSender(CHOOSE e \in s: TRUE)
           ELSE FALSE }
 tla {   ANY2(s,fieldname) ==
           LET ss ==
                 {val[fieldname]: val \in s}
            IN IF Assert(\A a,b \in ss: a = b,
                         <<"ANY2(s,fieldname), not all elements are equal!",
                           s,
                           fieldname,
                           ss>>)
               THEN CHOOSE e \in ss: TRUE
               ELSE FALSE }
 tla {   ALL(s) ==
           s }
 tla {   SENDERS(s) ==
           {m.sender: m \in ALL(s)} }



role R {
  state BOOLEAN xr = FALSE
   handle crash S s  {
      change xr = TRUE
  }
}
role S {
  state BOOLEAN xs = FALSE
   handle crash R r  {
      change xs = TRUE
  }
}
--------

-------- AFTER rewriteDoMeanwhile -----------------
 
 tla {   NullSender(msg) ==
           [msg EXCEPT !.sender = 0] }
 tla {   ANY(s) ==
           IF Assert(\A a,b \in s: NullSender(a) = NullSender(b),
                     <<
                     "ANY(s), not all elements (other than 'sender') are equal!",
                     s>>)
           THEN NullSender(CHOOSE e \in s: TRUE)
           ELSE FALSE }
 tla {   ANY2(s,fieldname) ==
           LET ss ==
                 {val[fieldname]: val \in s}
            IN IF Assert(\A a,b \in ss: a = b,
                         <<"ANY2(s,fieldname), not all elements are equal!",
                           s,
                           fieldname,
                           ss>>)
               THEN CHOOSE e \in ss: TRUE
               ELSE FALSE }
 tla {   ALL(s) ==
           s }
 tla {   SENDERS(s) ==
           {m.sender: m \in ALL(s)} }



role R {
  state BOOLEAN xr = FALSE
   handle crash S s  {
      change xr = TRUE
  }
}
role S {
  state BOOLEAN xs = FALSE
   handle crash R r  {
      change xs = TRUE
  }
}
--------

-------- AFTER rewriteCont ------------------------
 
 tla {   NullSender(msg) ==
           [msg EXCEPT !.sender = 0] }
 tla {   ANY(s) ==
           IF Assert(\A a,b \in s: NullSender(a) = NullSender(b),
                     <<
                     "ANY(s), not all elements (other than 'sender') are equal!",
                     s>>)
           THEN NullSender(CHOOSE e \in s: TRUE)
           ELSE FALSE }
 tla {   ANY2(s,fieldname) ==
           LET ss ==
                 {val[fieldname]: val \in s}
            IN IF Assert(\A a,b \in ss: a = b,
                         <<"ANY2(s,fieldname), not all elements are equal!",
                           s,
                           fieldname,
                           ss>>)
               THEN CHOOSE e \in ss: TRUE
               ELSE FALSE }
 tla {   ALL(s) ==
           s }
 tla {   SENDERS(s) ==
           {m.sender: m \in ALL(s)} }



role R {
  state BOOLEAN xr = FALSE
   handle crash S s  {
      change xr = TRUE
  }
}
role S {
  state BOOLEAN xs = FALSE
   handle crash R r  {
      change xs = TRUE
  }
}
--------

-------- AFTER rewriteONCE ------------------------
 
 tla {   NullSender(msg) ==
           [msg EXCEPT !.sender = 0] }
 tla {   ANY(s) ==
           IF Assert(\A a,b \in s: NullSender(a) = NullSender(b),
                     <<
                     "ANY(s), not all elements (other than 'sender') are equal!",
                     s>>)
           THEN NullSender(CHOOSE e \in s: TRUE)
           ELSE FALSE }
 tla {   ANY2(s,fieldname) ==
           LET ss ==
                 {val[fieldname]: val \in s}
            IN IF Assert(\A a,b \in ss: a = b,
                         <<"ANY2(s,fieldname), not all elements are equal!",
                           s,
                           fieldname,
                           ss>>)
               THEN CHOOSE e \in ss: TRUE
               ELSE FALSE }
 tla {   ALL(s) ==
           s }
 tla {   SENDERS(s) ==
           {m.sender: m \in ALL(s)} }



role R {
  state BOOLEAN xr = FALSE
   handle crash S s  {
      change xr = TRUE
  }
}
role S {
  state BOOLEAN xs = FALSE
   handle crash R r  {
      change xs = TRUE
  }
}
--------

-------- AFTER rewriteTimer -----------------------
 
 tla {   NullSender(msg) ==
           [msg EXCEPT !.sender = 0] }
 tla {   ANY(s) ==
           IF Assert(\A a,b \in s: NullSender(a) = NullSender(b),
                     <<
                     "ANY(s), not all elements (other than 'sender') are equal!",
                     s>>)
           THEN NullSender(CHOOSE e \in s: TRUE)
           ELSE FALSE }
 tla {   ANY2(s,fieldname) ==
           LET ss ==
                 {val[fieldname]: val \in s}
            IN IF Assert(\A a,b \in ss: a = b,
                         <<"ANY2(s,fieldname), not all elements are equal!",
                           s,
                           fieldname,
                           ss>>)
               THEN CHOOSE e \in ss: TRUE
               ELSE FALSE }
 tla {   ALL(s) ==
           s }
 tla {   SENDERS(s) ==
           {m.sender: m \in ALL(s)} }



role R {
  state BOOLEAN xr = FALSE
   handle crash S s  {
      change xr = TRUE
  }
}
role S {
  state BOOLEAN xs = FALSE
   handle crash R r  {
      change xs = TRUE
  }
}
--------

-------- AFTER RewriteMsgSetHandler ---------------
 
 tla {   NullSender(msg) ==
           [msg EXCEPT !.sender = 0] }
 tla {   ANY(s) ==
           IF Assert(\A a,b \in s: NullSender(a) = NullSender(b),
                     <<
                     "ANY(s), not all elements (other than 'sender') are equal!",
                     s>>)
           THEN NullSender(CHOOSE e \in s: TRUE)
           ELSE FALSE }
 tla {   ANY2(s,fieldname) ==
           LET ss ==
                 {val[fieldname]: val \in s}
            IN IF Assert(\A a,b \in ss: a = b,
                         <<"ANY2(s,fieldname), not all elements are equal!",
                           s,
                           fieldname,
                           ss>>)
               THEN CHOOSE e \in ss: TRUE
               ELSE FALSE }
 tla {   ALL(s) ==
           s }
 tla {   SENDERS(s) ==
           {m.sender: m \in ALL(s)} }



role R {
  state BOOLEAN xr = FALSE
   handle crash S s  {
      change xr = TRUE
  }
}
role S {
  state BOOLEAN xs = FALSE
   handle crash R r  {
      change xs = TRUE
  }
}
--------

-------- AFTER rewriteLifecycle -------------------
CONST  InitDownR,CrashR,StartR,InitDownS,CrashS,StartS
 tla {   NullSender(msg) ==
           [msg EXCEPT !.sender = 0] }
 tla {   ANY(s) ==
           IF Assert(\A a,b \in s: NullSender(a) = NullSender(b),
                     <<
                     "ANY(s), not all elements (other than 'sender') are equal!",
                     s>>)
           THEN NullSender(CHOOSE e \in s: TRUE)
           ELSE FALSE }
 tla {   ANY2(s,fieldname) ==
           LET ss ==
                 {val[fieldname]: val \in s}
            IN IF Assert(\A a,b \in ss: a = b,
                         <<"ANY2(s,fieldname), not all elements are equal!",
                           s,
                           fieldname,
                           ss>>)
               THEN CHOOSE e \in ss: TRUE
               ELSE FALSE }
 tla {   ALL(s) ==
           s }
 tla {   SENDERS(s) ==
           {m.sender: m \in ALL(s)} }
msg S -> Set<R> crashSR()
msg R -> Set<S> crashRS()


role GLOBAL {
  when /\ (\A s \in S: ~((st_S[s]).g_running))
       /\ (\A r \in R: ~((st_R[r]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role R {
  state BOOLEAN xr = FALSE
   when g_running handles msg crashSR   {
      let s = crashSR.sender
      change xr = TRUE
  }
  extend @hook_do_crash_R(self) {
      change xr = FALSE
  }
  state BOOLEAN g_running = ~(r \in InitDownR)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (r \in CrashR) handle event do_crash_R() @hook_do_crash_R(self) {
      change g_running = FALSE
      {local_s \in S: (st_S[local_s]).g_running} !! crashRS()
      change g_lifecycle = @+1
  }
  when /\ (~(g_running)) /\ (g_lifecycle < 2) handle event do_start_R() @
  hook_do_start_R
  (self) {
    | r \in StartR ->  change g_lifecycle = @+1
                       change g_running = TRUE
                       change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
role S {
  state BOOLEAN xs = FALSE
   when g_running handles msg crashRS   {
      let r = crashRS.sender
      change xs = TRUE
  }
  extend @hook_do_crash_S(self) {
      change xs = FALSE
  }
  state BOOLEAN g_running = ~(s \in InitDownS)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (s \in CrashS) handle event do_crash_S() @hook_do_crash_S(self) {
      change g_running = FALSE
      {local_r \in R: (st_R[local_r]).g_running} !! crashSR()
      change g_lifecycle = @+1
  }
  when /\ (~(g_running)) /\ (g_lifecycle < 2) handle event do_start_S() @
  hook_do_start_S
  (self) {
    | s \in StartS ->  change g_lifecycle = @+1
                       change g_running = TRUE
                       change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
--------

-------- AFTER groupSendInstr ---------------------
CONST  InitDownR,CrashR,StartR,InitDownS,CrashS,StartS
 tla {   NullSender(msg) ==
           [msg EXCEPT !.sender = 0] }
 tla {   ANY(s) ==
           IF Assert(\A a,b \in s: NullSender(a) = NullSender(b),
                     <<
                     "ANY(s), not all elements (other than 'sender') are equal!",
                     s>>)
           THEN NullSender(CHOOSE e \in s: TRUE)
           ELSE FALSE }
 tla {   ANY2(s,fieldname) ==
           LET ss ==
                 {val[fieldname]: val \in s}
            IN IF Assert(\A a,b \in ss: a = b,
                         <<"ANY2(s,fieldname), not all elements are equal!",
                           s,
                           fieldname,
                           ss>>)
               THEN CHOOSE e \in ss: TRUE
               ELSE FALSE }
 tla {   ALL(s) ==
           s }
 tla {   SENDERS(s) ==
           {m.sender: m \in ALL(s)} }
msg S -> Set<R> crashSR()
msg R -> Set<S> crashRS()


role GLOBAL {
  when /\ (\A s \in S: ~((st_S[s]).g_running))
       /\ (\A r \in R: ~((st_R[r]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role R {
  state BOOLEAN xr = FALSE
   when g_running handles msg crashSR   {
      let s = crashSR.sender
      change xr = TRUE
  }
  extend @hook_do_crash_R(self) {
      change xr = FALSE
  }
  state BOOLEAN g_running = ~(r \in InitDownR)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (r \in CrashR) handle event do_crash_R() @hook_do_crash_R(self) {
      change g_running = FALSE
      change g_lifecycle = @+1
      send-group {local_s \in S: (st_S[local_s]).g_running} !! crashRS()
  }
  when /\ (~(g_running)) /\ (g_lifecycle < 2) handle event do_start_R() @
  hook_do_start_R
  (self) {
    | r \in StartR ->  change g_lifecycle = @+1
                       change g_running = TRUE
                       change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
role S {
  state BOOLEAN xs = FALSE
   when g_running handles msg crashRS   {
      let r = crashRS.sender
      change xs = TRUE
  }
  extend @hook_do_crash_S(self) {
      change xs = FALSE
  }
  state BOOLEAN g_running = ~(s \in InitDownS)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (s \in CrashS) handle event do_crash_S() @hook_do_crash_S(self) {
      change g_running = FALSE
      change g_lifecycle = @+1
      send-group {local_r \in R: (st_R[local_r]).g_running} !! crashSR()
  }
  when /\ (~(g_running)) /\ (g_lifecycle < 2) handle event do_start_S() @
  hook_do_start_S
  (self) {
    | s \in StartS ->  change g_lifecycle = @+1
                       change g_running = TRUE
                       change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
--------

-------- AFTER RewriteExtendHook ------------------
CONST  InitDownR,CrashR,StartR,InitDownS,CrashS,StartS
 tla {   NullSender(msg) ==
           [msg EXCEPT !.sender = 0] }
 tla {   ANY(s) ==
           IF Assert(\A a,b \in s: NullSender(a) = NullSender(b),
                     <<
                     "ANY(s), not all elements (other than 'sender') are equal!",
                     s>>)
           THEN NullSender(CHOOSE e \in s: TRUE)
           ELSE FALSE }
 tla {   ANY2(s,fieldname) ==
           LET ss ==
                 {val[fieldname]: val \in s}
            IN IF Assert(\A a,b \in ss: a = b,
                         <<"ANY2(s,fieldname), not all elements are equal!",
                           s,
                           fieldname,
                           ss>>)
               THEN CHOOSE e \in ss: TRUE
               ELSE FALSE }
 tla {   ALL(s) ==
           s }
 tla {   SENDERS(s) ==
           {m.sender: m \in ALL(s)} }
msg S -> Set<R> crashSR()
msg R -> Set<S> crashRS()


role GLOBAL {
  when /\ (\A s \in S: ~((st_S[s]).g_running))
       /\ (\A r \in R: ~((st_R[r]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role R {
  state BOOLEAN xr = FALSE
   when g_running handles msg crashSR   {
      let s = crashSR.sender
      change xr = TRUE
  }
  state BOOLEAN g_running = ~(r \in InitDownR)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (r \in CrashR) handle event do_crash_R()  {
      change g_running = FALSE
      change g_lifecycle = @+1
      send-group {local_s \in S: (st_S[local_s]).g_running} !! crashRS()
      change xr = FALSE
  }
  when /\ (~(g_running)) /\ (g_lifecycle < 2) handle event do_start_R
  ()  {
    | r \in StartR ->  change g_lifecycle = @+1
                       change g_running = TRUE
                       change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
role S {
  state BOOLEAN xs = FALSE
   when g_running handles msg crashRS   {
      let r = crashRS.sender
      change xs = TRUE
  }
  state BOOLEAN g_running = ~(s \in InitDownS)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (s \in CrashS) handle event do_crash_S()  {
      change g_running = FALSE
      change g_lifecycle = @+1
      send-group {local_r \in R: (st_R[local_r]).g_running} !! crashSR()
      change xs = FALSE
  }
  when /\ (~(g_running)) /\ (g_lifecycle < 2) handle event do_start_S
  ()  {
    | s \in StartS ->  change g_lifecycle = @+1
                       change g_running = TRUE
                       change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
--------

-------- AFTER Rewrite special operators ----------
CONST  InitDownR,CrashR,StartR,InitDownS,CrashS,StartS
 tla {   NullSender(msg) ==
           [msg EXCEPT !.sender = 0] }
 tla {   ANY(s) ==
           IF Assert(\A a,b \in s: NullSender(a) = NullSender(b),
                     <<
                     "ANY(s), not all elements (other than 'sender') are equal!",
                     s>>)
           THEN NullSender(CHOOSE e \in s: TRUE)
           ELSE FALSE }
 tla {   ANY2(s,fieldname) ==
           LET ss ==
                 {val[fieldname]: val \in s}
            IN IF Assert(\A a,b \in ss: a = b,
                         <<"ANY2(s,fieldname), not all elements are equal!",
                           s,
                           fieldname,
                           ss>>)
               THEN CHOOSE e \in ss: TRUE
               ELSE FALSE }
 tla {   ALL(s) ==
           s }
 tla {   SENDERS(s) ==
           {m.sender: m \in ALL(s)} }
msg S -> Set<R> crashSR()
msg R -> Set<S> crashRS()


role GLOBAL {
  when /\ (\A s \in S: ~((st_S[s]).g_running))
       /\ (\A r \in R: ~((st_R[r]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role R {
  state BOOLEAN xr = FALSE
   when g_running handles msg crashSR   {
      let s = crashSR.sender
      change xr = TRUE
  }
  state BOOLEAN g_running = ~(r \in InitDownR)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (r \in CrashR) handle event do_crash_R()  {
      change g_running = FALSE
      change g_lifecycle = @+1
      send-group {local_s \in S: (st_S[local_s]).g_running} !! crashRS()
      change xr = FALSE
  }
  when /\ (~(g_running)) /\ (g_lifecycle < 2) handle event do_start_R
  ()  {
    | r \in StartR ->  change g_lifecycle = @+1
                       change g_running = TRUE
                       change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
role S {
  state BOOLEAN xs = FALSE
   when g_running handles msg crashRS   {
      let r = crashRS.sender
      change xs = TRUE
  }
  state BOOLEAN g_running = ~(s \in InitDownS)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (s \in CrashS) handle event do_crash_S()  {
      change g_running = FALSE
      change g_lifecycle = @+1
      send-group {local_r \in R: (st_R[local_r]).g_running} !! crashSR()
      change xs = FALSE
  }
  when /\ (~(g_running)) /\ (g_lifecycle < 2) handle event do_start_S
  ()  {
    | s \in StartS ->  change g_lifecycle = @+1
                       change g_running = TRUE
                       change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
--------

-------- AFTER RewriteOverrideTLA ----------
CONST  InitDownR,CrashR,StartR,InitDownS,CrashS,StartS
 tla {   NullSender(msg) ==
           [msg EXCEPT !.sender = 0] }
 tla {   ANY(s) ==
           IF Assert(\A a,b \in s: NullSender(a) = NullSender(b),
                     <<
                     "ANY(s), not all elements (other than 'sender') are equal!",
                     s>>)
           THEN NullSender(CHOOSE e \in s: TRUE)
           ELSE FALSE }
 tla {   ANY2(s,fieldname) ==
           LET ss ==
                 {val[fieldname]: val \in s}
            IN IF Assert(\A a,b \in ss: a = b,
                         <<"ANY2(s,fieldname), not all elements are equal!",
                           s,
                           fieldname,
                           ss>>)
               THEN CHOOSE e \in ss: TRUE
               ELSE FALSE }
 tla {   ALL(s) ==
           s }
 tla {   SENDERS(s) ==
           {m.sender: m \in ALL(s)} }
msg S -> Set<R> crashSR()
msg R -> Set<S> crashRS()


role GLOBAL {
  when /\ (\A s \in S: ~((st_S[s]).g_running))
       /\ (\A r \in R: ~((st_R[r]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role R {
  state BOOLEAN xr = FALSE
   when g_running handles msg crashSR   {
      let s = crashSR.sender
      change xr = TRUE
  }
  state BOOLEAN g_running = ~(r \in InitDownR)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (r \in CrashR) handle event do_crash_R()  {
      change g_running = FALSE
      change g_lifecycle = @+1
      send-group {local_s \in S: (st_S[local_s]).g_running} !! crashRS()
      change xr = FALSE
  }
  when /\ (~(g_running)) /\ (g_lifecycle < 2) handle event do_start_R
  ()  {
    | r \in StartR ->  change g_lifecycle = @+1
                       change g_running = TRUE
                       change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
role S {
  state BOOLEAN xs = FALSE
   when g_running handles msg crashRS   {
      let r = crashRS.sender
      change xs = TRUE
  }
  state BOOLEAN g_running = ~(s \in InitDownS)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (s \in CrashS) handle event do_crash_S()  {
      change g_running = FALSE
      change g_lifecycle = @+1
      send-group {local_r \in R: (st_R[local_r]).g_running} !! crashSR()
      change xs = FALSE
  }
  when /\ (~(g_running)) /\ (g_lifecycle < 2) handle event do_start_S
  ()  {
    | s \in StartS ->  change g_lifecycle = @+1
                       change g_running = TRUE
                       change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
--------


---- MODULE prim_startrole ----
EXTENDS TLC,Naturals,FiniteSets,Sequences
---- 
CONSTANT InitDownR,CrashR,StartR,InitDownS,CrashS,StartS,CrashSR_4242(_,_),CrashRS_4242(_,_),R,S 
VARIABLE st_GLOBAL,st_R,st_S 
---- 
SMultiSendToR(s) ==
  /\ (Len((st_S[s]).g_obuf) > 0 /\ LET e ==
                                         Head((st_S[s]).g_obuf) 
                                       m ==
                                         e[1] 
                                       d ==
                                         e[2]
                                    IN IF d = {}
                                       THEN /\ (st_S' = [st_S EXCEPT ![s].g_obuf = Tail(@)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>))
                                       ELSE \E p \in d: /\ (p \in R /\ IF Cardinality(d) = 1 THEN /\ (st_S' = [st_S EXCEPT ![s].g_obuf = Tail(@)] /\ st_R' = [st_R EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>)) ELSE /\ (st_S' = [st_S EXCEPT ![s].g_obuf = [@ EXCEPT ![1] = <<m,d \ {p}>>]] /\ st_R' = [st_R EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>)))) 
RMultiSendToS(r) ==
  /\ (Len((st_R[r]).g_obuf) > 0 /\ LET e ==
                                         Head((st_R[r]).g_obuf) 
                                       m ==
                                         e[1] 
                                       d ==
                                         e[2]
                                    IN IF d = {}
                                       THEN /\ (st_R' = [st_R EXCEPT ![r].g_obuf = Tail(@)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_S>>))
                                       ELSE \E p \in d: /\ (p \in S /\ IF Cardinality(d) = 1 THEN /\ (st_R' = [st_R EXCEPT ![r].g_obuf = Tail(@)] /\ st_S' = [st_S EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>)) ELSE /\ (st_R' = [st_R EXCEPT ![r].g_obuf = [@ EXCEPT ![1] = <<m,d \ {p}>>]] /\ st_S' = [st_S EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>)))) 
Majority(q,s) ==
  Cardinality(q) > Cardinality(s) \div 2 
MsgPos(seq,t,Precond(_,_),agent) ==
  IF \E i \in 1..Len(seq): /\ ((seq[i]).type = t /\ Precond(agent,seq[i]) /\ \A j \in 1..i-1: ~(/\ ((seq[j]).type = t /\ Precond(agent,seq[j])))) THEN CHOOSE i \in 1..Len(seq): /\ ((seq[i]).type = t /\ Precond(agent,seq[i]) /\ \A j \in 1..i-1: ~(/\ ((seq[j]).type = t /\ Precond(agent,seq[j])))) ELSE 0 
DropPos(seq,p) ==
  SubSeq(seq,1,p-1) \o SubSeq(seq,p+1,Len(seq)) 
---- 
NullSender(msg) ==
  [msg EXCEPT !.sender = 0] 
ANY(s) ==
  IF Assert(\A a,b \in s: NullSender(a) = NullSender(b),<<"ANY(s), not all elements (other than 'sender') are equal!",s>>) THEN NullSender(CHOOSE e \in s: TRUE) ELSE FALSE 
ANY2(s,fieldname) ==
  LET ss ==   {val[fieldname]: val \in s}  IN IF Assert(\A a,b \in ss: a = b,<<"ANY2(s,fieldname), not all elements are equal!",s,fieldname,ss>>) THEN CHOOSE e \in ss: TRUE ELSE FALSE 
ALL(s) ==
  s 
SENDERS(s) ==
  {m.sender: m \in ALL(s)} 
---- 
Msg ==
  [type: {"crashSR"},sender: S] \cup [type: {"crashRS"},sender: R] 
---- 
GLOBALState ==
  [g_dummy: BOOLEAN] 
RState ==
  [xr: BOOLEAN,g_running: BOOLEAN,g_lifecycle: {0,1,2},g_inbox: Seq(Msg),g_obuf: Seq(Msg \X (SUBSET (S)))] 
SState ==
  [xs: BOOLEAN,g_running: BOOLEAN,g_lifecycle: {0,1,2},g_inbox: Seq(Msg),g_obuf: Seq(Msg \X (SUBSET (R)))] 
---- 
TypeInvariant ==
  /\ (st_GLOBAL \in GLOBALState) /\ (st_R \in [R -> RState]) /\ (st_S \in [S -> SState]) 
Init ==
  /\ (st_GLOBAL = [g_dummy |-> FALSE]) /\ (st_R = [r \in R |-> [xr |-> FALSE,g_running |-> ~(r \in InitDownR),g_lifecycle |-> 0,g_inbox |-> <<>>,g_obuf |-> <<>>]]) /\ (st_S = [s \in S |-> [xs |-> FALSE,g_running |-> ~(s \in InitDownS),g_lifecycle |-> 0,g_inbox |-> <<>>,g_obuf |-> <<>>]]) 
---- 
StutterWhenNothingRunning_4242 ==
  /\ (/\ (\A s \in S: ~((st_S[s]).g_running)) /\ (\A r \in R: ~((st_R[r]).g_running))) /\ (/\ (UNCHANGED (<<st_GLOBAL,st_R,st_S>>))) 
ZzZCrashSR_4242(r,msgpos) ==
  /\ ((st_R[r]).g_running)
  /\ (Len((st_R[r]).g_inbox) > 0)
  /\ (LET Local_F(local_r,crashSR) ==   LET s ==   crashSR.sender  IN /\ (TRUE) /\ (TRUE) local_p ==   IF Head((st_R[r]).g_inbox).type = "crashSR" THEN 1 ELSE 0  IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET crashSR ==   (st_R[r]).g_inbox[local_p]  IN LET s ==   crashSR.sender  IN /\ (st_R' = [st_R EXCEPT ![r].xr = TRUE,![r].g_inbox = DropPos((st_R[r]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_GLOBAL,st_S>>)))) 
Do_crash_R_4242(r) ==
  /\ (/\ ((st_R[r]).g_running) /\ ((st_R[r]).g_lifecycle < 2) /\ (r \in CrashR)) /\ (/\ (st_R' = [st_R EXCEPT ![r].g_running = FALSE,![r].g_lifecycle = @+1,![r].g_obuf = <<<<[type |-> "crashRS",sender |-> r],{local_s \in S: (st_S[local_s]).g_running}>>>>,![r].xr = FALSE]) /\ (UNCHANGED (<<st_GLOBAL,st_S>>))) 
Do_start_R_4242(r) ==
  /\ (/\ (~((st_R[r]).g_running)) /\ ((st_R[r]).g_lifecycle < 2)) /\ (CASE r \in StartR -> /\ (st_R' = [st_R EXCEPT ![r].g_lifecycle = @+1,![r].g_running = TRUE,![r].g_inbox = <<>>]) /\ (UNCHANGED (<<st_GLOBAL,st_S>>)) [] OTHER -> /\ (FALSE) /\ (UNCHANGED (<<st_GLOBAL,st_R,st_S>>))) 
ZzZCrashRS_4242(s,msgpos) ==
  /\ ((st_S[s]).g_running)
  /\ (Len((st_S[s]).g_inbox) > 0)
  /\ (LET Local_F(local_s,crashRS) ==   LET r ==   crashRS.sender  IN /\ (TRUE) /\ (TRUE) local_p ==   IF Head((st_S[s]).g_inbox).type = "crashRS" THEN 1 ELSE 0  IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET crashRS ==   (st_S[s]).g_inbox[local_p]  IN LET r ==   crashRS.sender  IN /\ (st_S' = [st_S EXCEPT ![s].xs = TRUE,![s].g_inbox = DropPos((st_S[s]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_GLOBAL,st_R>>)))) 
Do_crash_S_4242(s) ==
  /\ (/\ ((st_S[s]).g_running) /\ ((st_S[s]).g_lifecycle < 2) /\ (s \in CrashS)) /\ (/\ (st_S' = [st_S EXCEPT ![s].g_running = FALSE,![s].g_lifecycle = @+1,![s].g_obuf = <<<<[type |-> "crashSR",sender |-> s],{local_r \in R: (st_R[local_r]).g_running}>>>>,![s].xs = FALSE]) /\ (UNCHANGED (<<st_GLOBAL,st_R>>))) 
Do_start_S_4242(s) ==
  /\ (/\ (~((st_S[s]).g_running)) /\ ((st_S[s]).g_lifecycle < 2)) /\ (CASE s \in StartS -> /\ (st_S' = [st_S EXCEPT ![s].g_lifecycle = @+1,![s].g_running = TRUE,![s].g_inbox = <<>>]) /\ (UNCHANGED (<<st_GLOBAL,st_R>>)) [] OTHER -> /\ (FALSE) /\ (UNCHANGED (<<st_GLOBAL,st_R,st_S>>))) 
---- 
Fairness ==
  /\ (WF_<<st_GLOBAL,st_R,st_S>>(StutterWhenNothingRunning_4242))
  /\ (WF_<<st_GLOBAL,st_R,st_S>>(\E r \in R: CrashSR_4242(r,0)))
  /\ (WF_<<st_GLOBAL,st_R,st_S>>(\E r \in R: Do_crash_R_4242(r)))
  /\ (WF_<<st_GLOBAL,st_R,st_S>>(\E r \in R: Do_start_R_4242(r)))
  /\ (WF_<<st_GLOBAL,st_R,st_S>>(\E s \in S: CrashRS_4242(s,0)))
  /\ (WF_<<st_GLOBAL,st_R,st_S>>(\E s \in S: Do_crash_S_4242(s)))
  /\ (WF_<<st_GLOBAL,st_R,st_S>>(\E s \in S: Do_start_S_4242(s)))
  /\ (WF_<<st_GLOBAL,st_R,st_S>>(\E s \in S: SMultiSendToR(s)))
  /\ (WF_<<st_GLOBAL,st_R,st_S>>(\E r \in R: RMultiSendToS(r))) 
---- 
Next ==
  \/ (StutterWhenNothingRunning_4242) \/ (\E r \in R: CrashSR_4242(r,0)) \/ (\E r \in R: Do_crash_R_4242(r)) \/ (\E r \in R: Do_start_R_4242(r)) \/ (\E s \in S: CrashRS_4242(s,0)) \/ (\E s \in S: Do_crash_S_4242(s)) \/ (\E s \in S: Do_start_S_4242(s)) \/ (\E s \in S: SMultiSendToR(s)) \/ (\E r \in R: RMultiSendToS(r)) 
---- 
Spec ==
  /\ (Init) /\ ([]([Next]_<<st_GLOBAL,st_R,st_S>>)) /\ (Fairness)
====
