---- PARTS ----
protocol prim_crash3 concern default {
  roles R,S
  interaction i {
    role R {
       handle crash X x  {
          change h1 = x
      }
       handle crash X y  {
          change h2 = y
      }
    }
    role S {
       when TRUE handles crash X x  {
          change h1 = x
      }
       when TRUE handles crash X y  {
          change h2 = y
      }
    }
    role T {
       when TRUE handles crash X x  {
          change h1 = x
      }
       when FALSE handles crash X y  {
          change h2 = y
      }
    }
    role U {
       handle crash Foo x  {
          change h1 = x
      }
       handle crash Bar y  {
          change h2 = y
      }
    }
  }
}
--------


-------- AFTER Merging interactions ---------------
protocol prim_crash3 concern __generated {
  roles R,S
  interaction __generated {
    role R {
       handle crash X x  {
          change h1 = x
      }
       handle crash X y  {
          change h2 = y
      }
    }
    role S {
       when TRUE handles crash X x  {
          change h1 = x
      }
       when TRUE handles crash X y  {
          change h2 = y
      }
    }
    role T {
       when TRUE handles crash X x  {
          change h1 = x
      }
       when FALSE handles crash X y  {
          change h2 = y
      }
    }
    role U {
       handle crash Foo x  {
          change h1 = x
      }
       handle crash Bar y  {
          change h2 = y
      }
    }
  }
}
--------

-------- AFTER insideOut (flatten) ----------------
 




role R {
   handle crash X x  {
      change h1 = x
  }
   handle crash X y  {
      change h2 = y
  }
}
role S {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when TRUE handles crash X y  {
      change h2 = y
  }
}
role T {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when FALSE handles crash X y  {
      change h2 = y
  }
}
role U {
   handle crash Foo x  {
      change h1 = x
  }
   handle crash Bar y  {
      change h2 = y
  }
}
--------

-------- AFTER rewriteWhen ------------------------
 




role R {
   handle crash X x  {
      change h1 = x
  }
   handle crash X y  {
      change h2 = y
  }
}
role S {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when TRUE handles crash X y  {
      change h2 = y
  }
}
role T {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when FALSE handles crash X y  {
      change h2 = y
  }
}
role U {
   handle crash Foo x  {
      change h1 = x
  }
   handle crash Bar y  {
      change h2 = y
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
   handle crash X x  {
      change h1 = x
  }
   handle crash X y  {
      change h2 = y
  }
}
role S {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when TRUE handles crash X y  {
      change h2 = y
  }
}
role T {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when FALSE handles crash X y  {
      change h2 = y
  }
}
role U {
   handle crash Foo x  {
      change h1 = x
  }
   handle crash Bar y  {
      change h2 = y
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
   handle crash X x  {
      change h1 = x
  }
   handle crash X y  {
      change h2 = y
  }
}
role S {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when TRUE handles crash X y  {
      change h2 = y
  }
}
role T {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when FALSE handles crash X y  {
      change h2 = y
  }
}
role U {
   handle crash Foo x  {
      change h1 = x
  }
   handle crash Bar y  {
      change h2 = y
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
   handle crash X x  {
      change h1 = x
  }
   handle crash X y  {
      change h2 = y
  }
}
role S {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when TRUE handles crash X y  {
      change h2 = y
  }
}
role T {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when FALSE handles crash X y  {
      change h2 = y
  }
}
role U {
   handle crash Foo x  {
      change h1 = x
  }
   handle crash Bar y  {
      change h2 = y
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
   handle crash X x  {
      change h1 = x
  }
   handle crash X y  {
      change h2 = y
  }
}
role S {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when TRUE handles crash X y  {
      change h2 = y
  }
}
role T {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when FALSE handles crash X y  {
      change h2 = y
  }
}
role U {
   handle crash Foo x  {
      change h1 = x
  }
   handle crash Bar y  {
      change h2 = y
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
   handle crash X x  {
      change h1 = x
  }
   handle crash X y  {
      change h2 = y
  }
}
role S {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when TRUE handles crash X y  {
      change h2 = y
  }
}
role T {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when FALSE handles crash X y  {
      change h2 = y
  }
}
role U {
   handle crash Foo x  {
      change h1 = x
  }
   handle crash Bar y  {
      change h2 = y
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
   handle crash X x  {
      change h1 = x
  }
   handle crash X y  {
      change h2 = y
  }
}
role S {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when TRUE handles crash X y  {
      change h2 = y
  }
}
role T {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when FALSE handles crash X y  {
      change h2 = y
  }
}
role U {
   handle crash Foo x  {
      change h1 = x
  }
   handle crash Bar y  {
      change h2 = y
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
   handle crash X x  {
      change h1 = x
  }
   handle crash X y  {
      change h2 = y
  }
}
role S {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when TRUE handles crash X y  {
      change h2 = y
  }
}
role T {
   when TRUE handles crash X x  {
      change h1 = x
  }
   when FALSE handles crash X y  {
      change h2 = y
  }
}
role U {
   handle crash Foo x  {
      change h1 = x
  }
   handle crash Bar y  {
      change h2 = y
  }
}
--------

-------- AFTER rewriteLifecycle -------------------
CONST  InitDownR,CrashR,StartR,InitDownS,CrashS,StartS,InitDownT,CrashT,StartT,InitDownU,CrashU,StartU
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
msg X -> Set<R> crashXR()
msg X -> Set<S> crashXS()
msg X -> Set<T> crashXT()
msg X -> Set<T> crashXT()
msg Foo -> Set<U> crashFooU()
msg Bar -> Set<U> crashBarU()


role GLOBAL {
  when /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A foo \in Foo: ~((st_Foo[foo]).g_running))
       /\ (\A bar \in Bar: ~((st_Bar[bar]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role R {
   when g_running handles msg crashXR   {
      let x = crashXR.sender
      change h1 = x
      let x = crashXR.sender
      change h2 = y
  }
  state BOOLEAN g_running = ~(r \in InitDownR)
  state {0,1,2} g_lifecycle = 0
}
role S {
   when /\ (g_running) /\ (TRUE) handles msg crashXS   {
      let x = crashXS.sender
      change h1 = x
      let x = crashXS.sender
      change h2 = y
  }
  state BOOLEAN g_running = ~(s \in InitDownS)
  state {0,1,2} g_lifecycle = 0
}
role T {
   when /\ (g_running) /\ (TRUE) handles msg crashXT   {
      let x = crashXT.sender
      change h1 = x
  }
   when /\ (g_running) /\ (FALSE) handles msg crashXT   {
      let y = crashXT.sender
      change h2 = y
  }
  state BOOLEAN g_running = ~(t \in InitDownT)
  state {0,1,2} g_lifecycle = 0
}
role U {
   when g_running handles msg crashFooU   {
      let x = crashFooU.sender
      change h1 = x
  }
   when g_running handles msg crashBarU   {
      let y = crashBarU.sender
      change h2 = y
  }
  state BOOLEAN g_running = ~(u \in InitDownU)
  state {0,1,2} g_lifecycle = 0
}
--------

-------- AFTER groupSendInstr ---------------------
CONST  InitDownR,CrashR,StartR,InitDownS,CrashS,StartS,InitDownT,CrashT,StartT,InitDownU,CrashU,StartU
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
msg X -> Set<R> crashXR()
msg X -> Set<S> crashXS()
msg X -> Set<T> crashXT()
msg X -> Set<T> crashXT()
msg Foo -> Set<U> crashFooU()
msg Bar -> Set<U> crashBarU()


role GLOBAL {
  when /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A foo \in Foo: ~((st_Foo[foo]).g_running))
       /\ (\A bar \in Bar: ~((st_Bar[bar]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role R {
   when g_running handles msg crashXR   {
      let x = crashXR.sender
      change h1 = x
      let x = crashXR.sender
      change h2 = y
  }
  state BOOLEAN g_running = ~(r \in InitDownR)
  state {0,1,2} g_lifecycle = 0
}
role S {
   when /\ (g_running) /\ (TRUE) handles msg crashXS   {
      let x = crashXS.sender
      change h1 = x
      let x = crashXS.sender
      change h2 = y
  }
  state BOOLEAN g_running = ~(s \in InitDownS)
  state {0,1,2} g_lifecycle = 0
}
role T {
   when /\ (g_running) /\ (TRUE) handles msg crashXT   {
      let x = crashXT.sender
      change h1 = x
  }
   when /\ (g_running) /\ (FALSE) handles msg crashXT   {
      let y = crashXT.sender
      change h2 = y
  }
  state BOOLEAN g_running = ~(t \in InitDownT)
  state {0,1,2} g_lifecycle = 0
}
role U {
   when g_running handles msg crashFooU   {
      let x = crashFooU.sender
      change h1 = x
  }
   when g_running handles msg crashBarU   {
      let y = crashBarU.sender
      change h2 = y
  }
  state BOOLEAN g_running = ~(u \in InitDownU)
  state {0,1,2} g_lifecycle = 0
}
--------

-------- AFTER RewriteExtendHook ------------------
CONST  InitDownR,CrashR,StartR,InitDownS,CrashS,StartS,InitDownT,CrashT,StartT,InitDownU,CrashU,StartU
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
msg X -> Set<R> crashXR()
msg X -> Set<S> crashXS()
msg X -> Set<T> crashXT()
msg X -> Set<T> crashXT()
msg Foo -> Set<U> crashFooU()
msg Bar -> Set<U> crashBarU()


role GLOBAL {
  when /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A foo \in Foo: ~((st_Foo[foo]).g_running))
       /\ (\A bar \in Bar: ~((st_Bar[bar]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role R {
   when g_running handles msg crashXR   {
      let x = crashXR.sender
      change h1 = x
      change h2 = y
  }
  state BOOLEAN g_running = ~(r \in InitDownR)
  state {0,1,2} g_lifecycle = 0
}
role S {
   when /\ (g_running) /\ (TRUE) handles msg crashXS   {
      let x = crashXS.sender
      change h1 = x
      change h2 = y
  }
  state BOOLEAN g_running = ~(s \in InitDownS)
  state {0,1,2} g_lifecycle = 0
}
role T {
   when /\ (g_running) /\ (TRUE) handles msg crashXT   {
      let x = crashXT.sender
      change h1 = x
  }
   when /\ (g_running) /\ (FALSE) handles msg crashXT   {
      let y = crashXT.sender
      change h2 = y
  }
  state BOOLEAN g_running = ~(t \in InitDownT)
  state {0,1,2} g_lifecycle = 0
}
role U {
   when g_running handles msg crashFooU   {
      let x = crashFooU.sender
      change h1 = x
  }
   when g_running handles msg crashBarU   {
      let y = crashBarU.sender
      change h2 = y
  }
  state BOOLEAN g_running = ~(u \in InitDownU)
  state {0,1,2} g_lifecycle = 0
}
--------

-------- AFTER Rewrite special operators ----------
CONST  InitDownR,CrashR,StartR,InitDownS,CrashS,StartS,InitDownT,CrashT,StartT,InitDownU,CrashU,StartU
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
msg X -> Set<R> crashXR()
msg X -> Set<S> crashXS()
msg X -> Set<T> crashXT()
msg X -> Set<T> crashXT()
msg Foo -> Set<U> crashFooU()
msg Bar -> Set<U> crashBarU()


role GLOBAL {
  when /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A foo \in Foo: ~((st_Foo[foo]).g_running))
       /\ (\A bar \in Bar: ~((st_Bar[bar]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role R {
   when g_running handles msg crashXR   {
      let x = crashXR.sender
      change h1 = x
      change h2 = y
  }
  state BOOLEAN g_running = ~(r \in InitDownR)
  state {0,1,2} g_lifecycle = 0
}
role S {
   when /\ (g_running) /\ (TRUE) handles msg crashXS   {
      let x = crashXS.sender
      change h1 = x
      change h2 = y
  }
  state BOOLEAN g_running = ~(s \in InitDownS)
  state {0,1,2} g_lifecycle = 0
}
role T {
   when /\ (g_running) /\ (TRUE) handles msg crashXT   {
      let x = crashXT.sender
      change h1 = x
  }
   when /\ (g_running) /\ (FALSE) handles msg crashXT   {
      let y = crashXT.sender
      change h2 = y
  }
  state BOOLEAN g_running = ~(t \in InitDownT)
  state {0,1,2} g_lifecycle = 0
}
role U {
   when g_running handles msg crashFooU   {
      let x = crashFooU.sender
      change h1 = x
  }
   when g_running handles msg crashBarU   {
      let y = crashBarU.sender
      change h2 = y
  }
  state BOOLEAN g_running = ~(u \in InitDownU)
  state {0,1,2} g_lifecycle = 0
}
--------

-------- AFTER RewriteOverrideTLA ----------
CONST  InitDownR,CrashR,StartR,InitDownS,CrashS,StartS,InitDownT,CrashT,StartT,InitDownU,CrashU,StartU
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
msg X -> Set<R> crashXR()
msg X -> Set<S> crashXS()
msg X -> Set<T> crashXT()
msg X -> Set<T> crashXT()
msg Foo -> Set<U> crashFooU()
msg Bar -> Set<U> crashBarU()


role GLOBAL {
  when /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A x \in X: ~((st_X[x]).g_running))
       /\ (\A foo \in Foo: ~((st_Foo[foo]).g_running))
       /\ (\A bar \in Bar: ~((st_Bar[bar]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role R {
   when g_running handles msg crashXR   {
      let x = crashXR.sender
      change h1 = x
      change h2 = y
  }
  state BOOLEAN g_running = ~(r \in InitDownR)
  state {0,1,2} g_lifecycle = 0
}
role S {
   when /\ (g_running) /\ (TRUE) handles msg crashXS   {
      let x = crashXS.sender
      change h1 = x
      change h2 = y
  }
  state BOOLEAN g_running = ~(s \in InitDownS)
  state {0,1,2} g_lifecycle = 0
}
role T {
   when /\ (g_running) /\ (TRUE) handles msg crashXT   {
      let x = crashXT.sender
      change h1 = x
  }
   when /\ (g_running) /\ (FALSE) handles msg crashXT   {
      let y = crashXT.sender
      change h2 = y
  }
  state BOOLEAN g_running = ~(t \in InitDownT)
  state {0,1,2} g_lifecycle = 0
}
role U {
   when g_running handles msg crashFooU   {
      let x = crashFooU.sender
      change h1 = x
  }
   when g_running handles msg crashBarU   {
      let y = crashBarU.sender
      change h2 = y
  }
  state BOOLEAN g_running = ~(u \in InitDownU)
  state {0,1,2} g_lifecycle = 0
}
--------


---- MODULE prim_crash3 ----
EXTENDS TLC,Naturals,FiniteSets,Sequences
---- 
CONSTANT InitDownR,CrashR,StartR,InitDownS,CrashS,StartS,InitDownT,CrashT,StartT,InitDownU,CrashU,StartU,CrashXR_4242(_,_),CrashXS_4242(_,_),CrashXT_4242(_,_),CrashFooU_4242(_,_),CrashBarU_4242(_,_),R,S,T,U 
VARIABLE st_GLOBAL,st_R,st_S,st_T,st_U 
---- 
XMultiSendToR(x) ==
  /\ (Len((st_X[x]).g_obuf) > 0 /\ LET e ==
                                         Head((st_X[x]).g_obuf) 
                                       m ==
                                         e[1] 
                                       d ==
                                         e[2]
                                    IN IF d = {}
                                       THEN /\ (st_X' = [st_X EXCEPT ![x].g_obuf = Tail(@)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_T>>) /\ UNCHANGED (<<st_U>>) /\ UNCHANGED (<<st_R>>))
                                       ELSE \E p \in d: /\ (p \in R /\ IF Cardinality(d) = 1
                                                                       THEN /\ (st_X' = [st_X EXCEPT ![x].g_obuf = Tail(@)] /\ st_R' = [st_R EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_T>>) /\ UNCHANGED (<<st_U>>))
                                                                       ELSE /\ (st_X' = [st_X EXCEPT ![x].g_obuf = [@ EXCEPT ![1] = <<m,d \ {p}>>]] /\ st_R' = [st_R EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_T>>) /\ UNCHANGED (<<st_U>>)))) 
XMultiSendToS(x) ==
  /\ (Len((st_X[x]).g_obuf) > 0 /\ LET e ==
                                         Head((st_X[x]).g_obuf) 
                                       m ==
                                         e[1] 
                                       d ==
                                         e[2]
                                    IN IF d = {}
                                       THEN /\ (st_X' = [st_X EXCEPT ![x].g_obuf = Tail(@)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_T>>) /\ UNCHANGED (<<st_U>>) /\ UNCHANGED (<<st_S>>))
                                       ELSE \E p \in d: /\ (p \in S /\ IF Cardinality(d) = 1
                                                                       THEN /\ (st_X' = [st_X EXCEPT ![x].g_obuf = Tail(@)] /\ st_S' = [st_S EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_T>>) /\ UNCHANGED (<<st_U>>))
                                                                       ELSE /\ (st_X' = [st_X EXCEPT ![x].g_obuf = [@ EXCEPT ![1] = <<m,d \ {p}>>]] /\ st_S' = [st_S EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_T>>) /\ UNCHANGED (<<st_U>>)))) 
XMultiSendToT(x) ==
  /\ (Len((st_X[x]).g_obuf) > 0 /\ LET e ==
                                         Head((st_X[x]).g_obuf) 
                                       m ==
                                         e[1] 
                                       d ==
                                         e[2]
                                    IN IF d = {}
                                       THEN /\ (st_X' = [st_X EXCEPT ![x].g_obuf = Tail(@)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_U>>) /\ UNCHANGED (<<st_T>>))
                                       ELSE \E p \in d: /\ (p \in T /\ IF Cardinality(d) = 1
                                                                       THEN /\ (st_X' = [st_X EXCEPT ![x].g_obuf = Tail(@)] /\ st_T' = [st_T EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_U>>))
                                                                       ELSE /\ (st_X' = [st_X EXCEPT ![x].g_obuf = [@ EXCEPT ![1] = <<m,d \ {p}>>]] /\ st_T' = [st_T EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_U>>)))) 
XMultiSendToT(x) ==
  /\ (Len((st_X[x]).g_obuf) > 0 /\ LET e ==
                                         Head((st_X[x]).g_obuf) 
                                       m ==
                                         e[1] 
                                       d ==
                                         e[2]
                                    IN IF d = {}
                                       THEN /\ (st_X' = [st_X EXCEPT ![x].g_obuf = Tail(@)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_U>>) /\ UNCHANGED (<<st_T>>))
                                       ELSE \E p \in d: /\ (p \in T /\ IF Cardinality(d) = 1
                                                                       THEN /\ (st_X' = [st_X EXCEPT ![x].g_obuf = Tail(@)] /\ st_T' = [st_T EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_U>>))
                                                                       ELSE /\ (st_X' = [st_X EXCEPT ![x].g_obuf = [@ EXCEPT ![1] = <<m,d \ {p}>>]] /\ st_T' = [st_T EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_U>>)))) 
FooMultiSendToU(foo) ==
  /\ (Len((st_Foo[foo]).g_obuf) > 0 /\ LET e ==
                                             Head((st_Foo[foo]).g_obuf) 
                                           m ==
                                             e[1] 
                                           d ==
                                             e[2]
                                        IN IF d = {}
                                           THEN /\ (st_Foo' = [st_Foo EXCEPT ![foo].g_obuf = Tail(@)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_T>>) /\ UNCHANGED (<<st_U>>))
                                           ELSE \E p \in d: /\ (p \in U /\ IF Cardinality(d) = 1
                                                                           THEN /\ (st_Foo' = [st_Foo EXCEPT ![foo].g_obuf = Tail(@)] /\ st_U' = [st_U EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_T>>))
                                                                           ELSE /\ (st_Foo' = [st_Foo EXCEPT ![foo].g_obuf = [@ EXCEPT ![1] = <<m,d \ {p}>>]] /\ st_U' = [st_U EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_T>>)))) 
BarMultiSendToU(bar) ==
  /\ (Len((st_Bar[bar]).g_obuf) > 0 /\ LET e ==
                                             Head((st_Bar[bar]).g_obuf) 
                                           m ==
                                             e[1] 
                                           d ==
                                             e[2]
                                        IN IF d = {}
                                           THEN /\ (st_Bar' = [st_Bar EXCEPT ![bar].g_obuf = Tail(@)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_T>>) /\ UNCHANGED (<<st_U>>))
                                           ELSE \E p \in d: /\ (p \in U /\ IF Cardinality(d) = 1
                                                                           THEN /\ (st_Bar' = [st_Bar EXCEPT ![bar].g_obuf = Tail(@)] /\ st_U' = [st_U EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_T>>))
                                                                           ELSE /\ (st_Bar' = [st_Bar EXCEPT ![bar].g_obuf = [@ EXCEPT ![1] = <<m,d \ {p}>>]] /\ st_U' = [st_U EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_R>>) /\ UNCHANGED (<<st_S>>) /\ UNCHANGED (<<st_T>>)))) 
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
  [type: {"crashXR"},sender: X] \cup [type: {"crashXS"},sender: X] \cup [type: {"crashXT"},sender: X] \cup [type: {"crashXT"},sender: X] \cup [type: {"crashFooU"},sender: Foo] \cup [type: {"crashBarU"},sender: Bar] 
---- 
GLOBALState ==
  [g_dummy: BOOLEAN] 
RState ==
  [g_running: BOOLEAN,g_lifecycle: {0,1,2},g_inbox: Seq(Msg)] 
SState ==
  [g_running: BOOLEAN,g_lifecycle: {0,1,2},g_inbox: Seq(Msg)] 
TState ==
  [g_running: BOOLEAN,g_lifecycle: {0,1,2},g_inbox: Seq(Msg)] 
UState ==
  [g_running: BOOLEAN,g_lifecycle: {0,1,2},g_inbox: Seq(Msg)] 
---- 
TypeInvariant ==
  /\ (st_GLOBAL \in GLOBALState) /\ (st_R \in [R -> RState]) /\ (st_S \in [S -> SState]) /\ (st_T \in [T -> TState]) /\ (st_U \in [U -> UState]) 
Init ==
  /\ (st_GLOBAL = [g_dummy |-> FALSE]) /\ (st_R = [r \in R |-> [g_running |-> ~(r \in InitDownR),g_lifecycle |-> 0,g_inbox |-> <<>>]]) /\ (st_S = [s \in S |-> [g_running |-> ~(s \in InitDownS),g_lifecycle |-> 0,g_inbox |-> <<>>]]) /\ (st_T = [t \in T |-> [g_running |-> ~(t \in InitDownT),g_lifecycle |-> 0,g_inbox |-> <<>>]]) /\ (st_U = [u \in U |-> [g_running |-> ~(u \in InitDownU),g_lifecycle |-> 0,g_inbox |-> <<>>]]) 
---- 
StutterWhenNothingRunning_4242 ==
  /\ (/\ (\A x \in X: ~((st_X[x]).g_running)) /\ (\A x \in X: ~((st_X[x]).g_running)) /\ (\A x \in X: ~((st_X[x]).g_running)) /\ (\A x \in X: ~((st_X[x]).g_running)) /\ (\A x \in X: ~((st_X[x]).g_running)) /\ (\A x \in X: ~((st_X[x]).g_running)) /\ (\A foo \in Foo: ~((st_Foo[foo]).g_running)) /\ (\A bar \in Bar: ~((st_Bar[bar]).g_running))) /\ (/\ (UNCHANGED (<<st_GLOBAL,st_R,st_S,st_T,st_U>>))) 
ZzZCrashXR_4242(r,msgpos) ==
  /\ ((st_R[r]).g_running)
  /\ (Len((st_R[r]).g_inbox) > 0)
  /\ (LET Local_F(local_r,crashXR) ==
            LET x ==   crashXR.sender  IN /\ (TRUE) /\ (TRUE) 
          local_p ==
            IF Head((st_R[r]).g_inbox).type = "crashXR" THEN 1 ELSE 0
       IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET crashXR ==   (st_R[r]).g_inbox[local_p]  IN LET x ==   crashXR.sender  IN /\ (st_R' = [st_R EXCEPT ![r].h1 = x,![r].h2 = y,![r].g_inbox = DropPos((st_R[r]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_GLOBAL,st_S,st_T,st_U>>)))) 
ZzZCrashXS_4242(s,msgpos) ==
  /\ (/\ ((st_S[s]).g_running) /\ (TRUE))
  /\ (Len((st_S[s]).g_inbox) > 0)
  /\ (LET Local_F(local_s,crashXS) ==
            LET x ==   crashXS.sender  IN /\ (TRUE) /\ (TRUE) 
          local_p ==
            IF Head((st_S[s]).g_inbox).type = "crashXS" THEN 1 ELSE 0
       IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET crashXS ==   (st_S[s]).g_inbox[local_p]  IN LET x ==   crashXS.sender  IN /\ (st_S' = [st_S EXCEPT ![s].h1 = x,![s].h2 = y,![s].g_inbox = DropPos((st_S[s]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_GLOBAL,st_R,st_T,st_U>>)))) 
ZzZCrashXT_4242(t,msgpos) ==
  /\ (/\ ((st_T[t]).g_running) /\ (TRUE))
  /\ (Len((st_T[t]).g_inbox) > 0)
  /\ (LET Local_F(local_t,crashXT) ==   LET x ==   crashXT.sender  IN /\ (TRUE) /\ (TRUE) local_p ==   IF Head((st_T[t]).g_inbox).type = "crashXT" THEN 1 ELSE 0  IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET crashXT ==   (st_T[t]).g_inbox[local_p]  IN LET x ==   crashXT.sender  IN /\ (st_T' = [st_T EXCEPT ![t].h1 = x,![t].g_inbox = DropPos((st_T[t]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_GLOBAL,st_R,st_S,st_U>>)))) 
ZzZCrashXT_4242(t,msgpos) ==
  /\ (/\ ((st_T[t]).g_running) /\ (FALSE))
  /\ (Len((st_T[t]).g_inbox) > 0)
  /\ (LET Local_F(local_t,crashXT) ==   LET y ==   crashXT.sender  IN /\ (TRUE) /\ (TRUE) local_p ==   IF Head((st_T[t]).g_inbox).type = "crashXT" THEN 1 ELSE 0  IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET crashXT ==   (st_T[t]).g_inbox[local_p]  IN LET y ==   crashXT.sender  IN /\ (st_T' = [st_T EXCEPT ![t].h2 = y,![t].g_inbox = DropPos((st_T[t]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_GLOBAL,st_R,st_S,st_U>>)))) 
ZzZCrashFooU_4242(u,msgpos) ==
  /\ ((st_U[u]).g_running)
  /\ (Len((st_U[u]).g_inbox) > 0)
  /\ (LET Local_F(local_u,crashFooU) ==
            LET x ==   crashFooU.sender  IN /\ (TRUE) /\ (TRUE) 
          local_p ==
            IF Head((st_U[u]).g_inbox).type = "crashFooU" THEN 1 ELSE 0
       IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET crashFooU ==   (st_U[u]).g_inbox[local_p]  IN LET x ==   crashFooU.sender  IN /\ (st_U' = [st_U EXCEPT ![u].h1 = x,![u].g_inbox = DropPos((st_U[u]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_GLOBAL,st_R,st_S,st_T>>)))) 
ZzZCrashBarU_4242(u,msgpos) ==
  /\ ((st_U[u]).g_running)
  /\ (Len((st_U[u]).g_inbox) > 0)
  /\ (LET Local_F(local_u,crashBarU) ==
            LET y ==   crashBarU.sender  IN /\ (TRUE) /\ (TRUE) 
          local_p ==
            IF Head((st_U[u]).g_inbox).type = "crashBarU" THEN 1 ELSE 0
       IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET crashBarU ==   (st_U[u]).g_inbox[local_p]  IN LET y ==   crashBarU.sender  IN /\ (st_U' = [st_U EXCEPT ![u].h2 = y,![u].g_inbox = DropPos((st_U[u]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_GLOBAL,st_R,st_S,st_T>>)))) 
---- 
Fairness ==
  /\ (WF_<<st_GLOBAL,st_R,st_S,st_T,st_U>>(StutterWhenNothingRunning_4242))
  /\ (WF_<<st_GLOBAL,st_R,st_S,st_T,st_U>>(\E r \in R: CrashXR_4242(r,0)))
  /\ (WF_<<st_GLOBAL,st_R,st_S,st_T,st_U>>(\E s \in S: CrashXS_4242(s,0)))
  /\ (WF_<<st_GLOBAL,st_R,st_S,st_T,st_U>>(\E t \in T: CrashXT_4242(t,0)))
  /\ (WF_<<st_GLOBAL,st_R,st_S,st_T,st_U>>(\E t \in T: CrashXT_4242(t,0)))
  /\ (WF_<<st_GLOBAL,st_R,st_S,st_T,st_U>>(\E u \in U: CrashFooU_4242(u,0)))
  /\ (WF_<<st_GLOBAL,st_R,st_S,st_T,st_U>>(\E u \in U: CrashBarU_4242(u,0)))
  /\ (WF_<<st_GLOBAL,st_R,st_S,st_T,st_U>>(\E x \in X: XMultiSendToR(x)))
  /\ (WF_<<st_GLOBAL,st_R,st_S,st_T,st_U>>(\E x \in X: XMultiSendToS(x)))
  /\ (WF_<<st_GLOBAL,st_R,st_S,st_T,st_U>>(\E x \in X: XMultiSendToT(x)))
  /\ (WF_<<st_GLOBAL,st_R,st_S,st_T,st_U>>(\E x \in X: XMultiSendToT(x)))
  /\ (WF_<<st_GLOBAL,st_R,st_S,st_T,st_U>>(\E foo \in Foo: FooMultiSendToU(foo)))
  /\ (WF_<<st_GLOBAL,st_R,st_S,st_T,st_U>>(\E bar \in Bar: BarMultiSendToU(bar))) 
---- 
Next ==
  \/ (StutterWhenNothingRunning_4242)
  \/ (\E r \in R: CrashXR_4242(r,0))
  \/ (\E s \in S: CrashXS_4242(s,0))
  \/ (\E t \in T: CrashXT_4242(t,0))
  \/ (\E t \in T: CrashXT_4242(t,0))
  \/ (\E u \in U: CrashFooU_4242(u,0))
  \/ (\E u \in U: CrashBarU_4242(u,0))
  \/ (\E x \in X: XMultiSendToR(x))
  \/ (\E x \in X: XMultiSendToS(x))
  \/ (\E x \in X: XMultiSendToT(x))
  \/ (\E x \in X: XMultiSendToT(x))
  \/ (\E foo \in Foo: FooMultiSendToU(foo))
  \/ (\E bar \in Bar: BarMultiSendToU(bar)) 
---- 
Spec ==
  /\ (Init) /\ ([]([Next]_<<st_GLOBAL,st_R,st_S,st_T,st_U>>)) /\ (Fairness)
====
