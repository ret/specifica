---- PARTS ----
protocol lu concern default {
  roles Client,Server
  constant NIL,init_Server
  interaction core {
    msg Client -> Set<Server> prepare(BOOLEAN val)
    msg Server -> Client prepareAck(BOOLEAN locked)
    msg Client -> Set<Server> commit()
    msg Server -> Client commitAck()
    msg Client -> Set<Server> rd()
    msg Server -> Client rdRes(BOOLEAN val)
    display swimlane {
      msg prepare color = blue
      msg prepareAck color = IF locked THEN green ELSE red
      msg prepareAck style = IF locked THEN solid ELSE dashed
    }
     tla {   rdVal(rdResSet) ==
               any2(rdResSet,"val") }
    role Client {
      state views Set<Server> = init_Server
      handle event rdwr(BOOLEAN v)  {
          view(Server) !! rd()
           await msg rdRes from all(Server)   
          let newVal = ~(rdVal(rdRes))
          view(Server) !! prepare(val = newVal)
           await msg prepareAck from all(Server) where all prepareAck.locked  
          view(Server) !! commit()
           await msg commitAck from all(Server)   
      }
    }
    role Server {
      state BOOLEAN cvalue = FALSE
      state Nil<BOOLEAN> pvalue = NIL
       handle msg prepare   {
        | pvalue = NIL ->  change pvalue = prepare.val
                           reply prepareAck(locked = TRUE)
        | pvalue # NIL ->  reply prepareAck(locked = FALSE)
      }
      using [selective_receive] handle msg commit   {
          assert (pvalue # NIL,"Commit is missing previous prepare.")
          change cvalue = pvalue,pvalue = NIL
          reply commitAck()
      }
       handle msg rd   {
          reply rdRes(val = cvalue)
      }
    }
  }
  interaction deadlockHandling {
    msg Client -> Set<Server> release()
    display swimlane {
      msg release color = yellow
    }
    role Client {
      use msg prepareAck of core
       handle msg prepareAck from all(Server) where some ~(prepareAck.
      locked)  {
          let lockOK = {m.sender: m \in {m \in all(prepareAck): m.locked}}
          lockOK !! release()
      }
    }
    role Server {
      use state pvalue of core
       handle msg release   {
          change pvalue = NIL
      }
    }
  }
  interaction check_these {
     tla {   Consistency ==
               \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[
                                     b]).pvalue = NIL
                                  THEN (st_Server[a]).cvalue = (st_Server[b]).
                                       cvalue
                                  ELSE TRUE }
  }
}
--------


-------- AFTER Merging interactions ---------------
protocol lu concern __generated {
  constant NIL,init_Server
  roles Client,Server
  interaction __generated {
    msg Client -> Set<Server> prepare(BOOLEAN val)
    msg Server -> Client prepareAck(BOOLEAN locked)
    msg Client -> Set<Server> commit()
    msg Server -> Client commitAck()
    msg Client -> Set<Server> rd()
    msg Server -> Client rdRes(BOOLEAN val)
    msg Client -> Set<Server> release()
    display swimlane {
      msg prepare color = blue
      msg prepareAck color = IF locked THEN green ELSE red
      msg prepareAck style = IF locked THEN solid ELSE dashed
    }
    display swimlane {
      msg release color = yellow
    }
     tla {   rdVal(rdResSet) ==
               any2(rdResSet,"val") }
     tla {   Consistency ==
               \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[
                                     b]).pvalue = NIL
                                  THEN (st_Server[a]).cvalue = (st_Server[b]).
                                       cvalue
                                  ELSE TRUE }
    role Client {
      state views Set<Server> = init_Server
      handle event rdwr(BOOLEAN v)  {
          view(Server) !! rd()
           await msg rdRes from all(Server)   
          let newVal = ~(rdVal(rdRes))
          view(Server) !! prepare(val = newVal)
           await msg prepareAck from all(Server) where all prepareAck.locked  
          view(Server) !! commit()
           await msg commitAck from all(Server)   
      }
      use msg prepareAck of core
       handle msg prepareAck from all(Server) where some ~(prepareAck.
      locked)  {
          let lockOK = {m.sender: m \in {m \in all(prepareAck): m.locked}}
          lockOK !! release()
      }
    }
    role Server {
      state BOOLEAN cvalue = FALSE
      state Nil<BOOLEAN> pvalue = NIL
       handle msg prepare   {
        | pvalue = NIL ->  change pvalue = prepare.val
                           reply prepareAck(locked = TRUE)
        | pvalue # NIL ->  reply prepareAck(locked = FALSE)
      }
      using [selective_receive] handle msg commit   {
          assert (pvalue # NIL,"Commit is missing previous prepare.")
          change cvalue = pvalue,pvalue = NIL
          reply commitAck()
      }
       handle msg rd   {
          reply rdRes(val = cvalue)
      }
      use state pvalue of core
       handle msg release   {
          change pvalue = NIL
      }
    }
  }
}
--------

-------- AFTER insideOut (flatten) ----------------
CONST  NIL,init_Server
 tla {   rdVal(rdResSet) ==
           any2(rdResSet,"val") }
 tla {   Consistency ==
           \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).
                                 pvalue = NIL
                              THEN (st_Server[a]).cvalue = (st_Server[b]).
                                   cvalue
                              ELSE TRUE }
msg Client -> Set<Server> prepare(BOOLEAN val)
msg Server -> Client prepareAck(BOOLEAN locked)
msg Client -> Set<Server> commit()
msg Server -> Client commitAck()
msg Client -> Set<Server> rd()
msg Server -> Client rdRes(BOOLEAN val)
msg Client -> Set<Server> release()

display swimlane {
  msg prepare color = blue
  msg prepareAck color = IF locked THEN green ELSE red
  msg prepareAck style = IF locked THEN solid ELSE dashed
}
display swimlane {
  msg release color = yellow
}
role Client {
  state views Set<Server> = init_Server
  handle event rdwr(BOOLEAN v)  {
      view(Server) !! rd()
       await msg rdRes from all(Server)   
      let newVal = ~(rdVal(rdRes))
      view(Server) !! prepare(val = newVal)
       await msg prepareAck from all(Server) where all prepareAck.locked  
      view(Server) !! commit()
       await msg commitAck from all(Server)   
  }
  use msg prepareAck of core
   handle msg prepareAck from all(Server) where some ~(prepareAck.
  locked)  {
      let lockOK = {m.sender: m \in {m \in all(prepareAck): m.locked}}
      lockOK !! release()
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
  state Nil<BOOLEAN> pvalue = NIL
   handle msg prepare   {
    | pvalue = NIL ->  change pvalue = prepare.val
                       reply prepareAck(locked = TRUE)
    | pvalue # NIL ->  reply prepareAck(locked = FALSE)
  }
  using [selective_receive] handle msg commit   {
      assert (pvalue # NIL,"Commit is missing previous prepare.")
      change cvalue = pvalue,pvalue = NIL
      reply commitAck()
  }
   handle msg rd   {
      reply rdRes(val = cvalue)
  }
  use state pvalue of core
   handle msg release   {
      change pvalue = NIL
  }
}
--------

-------- AFTER rewriteWhen ------------------------
CONST  NIL,init_Server
 tla {   rdVal(rdResSet) ==
           any2(rdResSet,"val") }
 tla {   Consistency ==
           \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).
                                 pvalue = NIL
                              THEN (st_Server[a]).cvalue = (st_Server[b]).
                                   cvalue
                              ELSE TRUE }
msg Client -> Set<Server> prepare(BOOLEAN val)
msg Server -> Client prepareAck(BOOLEAN locked)
msg Client -> Set<Server> commit()
msg Server -> Client commitAck()
msg Client -> Set<Server> rd()
msg Server -> Client rdRes(BOOLEAN val)
msg Client -> Set<Server> release()

display swimlane {
  msg prepare color = blue
  msg prepareAck color = IF locked THEN green ELSE red
  msg prepareAck style = IF locked THEN solid ELSE dashed
}
display swimlane {
  msg release color = yellow
}
role Client {
  state views Set<Server> = init_Server
  handle event rdwr(BOOLEAN v)  {
      view(Server) !! rd()
       await msg rdRes from all(Server)   
      let newVal = ~(rdVal(rdRes))
      view(Server) !! prepare(val = newVal)
       await msg prepareAck from all(Server) where all prepareAck.locked  
      view(Server) !! commit()
       await msg commitAck from all(Server)   
  }
  use msg prepareAck of core
   handle msg prepareAck from all(Server) where some ~(prepareAck.
  locked)  {
      let lockOK = {m.sender: m \in {m \in all(prepareAck): m.locked}}
      lockOK !! release()
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
  state Nil<BOOLEAN> pvalue = NIL
   handle msg prepare   {
    | pvalue = NIL ->  change pvalue = prepare.val
                       reply prepareAck(locked = TRUE)
    | pvalue # NIL ->  reply prepareAck(locked = FALSE)
  }
  using [selective_receive] handle msg commit   {
      assert (pvalue # NIL,"Commit is missing previous prepare.")
      change cvalue = pvalue,pvalue = NIL
      reply commitAck()
  }
   handle msg rd   {
      reply rdRes(val = cvalue)
  }
  use state pvalue of core
   handle msg release   {
      change pvalue = NIL
  }
}
--------

-------- AFTER rewriteTAG -------------------------
CONST  NIL,init_Server
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
 tla {   rdVal(rdResSet) ==
           any2(rdResSet,"val") }
 tla {   Consistency ==
           \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).
                                 pvalue = NIL
                              THEN (st_Server[a]).cvalue = (st_Server[b]).
                                   cvalue
                              ELSE TRUE }
msg Client -> Set<Server> prepare(BOOLEAN val)
msg Server -> Client prepareAck(BOOLEAN locked)
msg Client -> Set<Server> commit()
msg Server -> Client commitAck()
msg Client -> Set<Server> rd()
msg Server -> Client rdRes(BOOLEAN val)
msg Client -> Set<Server> release()

display swimlane {
  msg prepare color = blue
  msg prepareAck color = IF locked THEN green ELSE red
  msg prepareAck style = IF locked THEN solid ELSE dashed
}
display swimlane {
  msg release color = yellow
}
role Client {
  state views Set<Server> = init_Server
  handle event rdwr(BOOLEAN v)  {
      view(Server) !! rd()
       await msg rdRes from all(Server)   
      let newVal = ~(rdVal(rdRes))
      view(Server) !! prepare(val = newVal)
       await msg prepareAck from all(Server) where all prepareAck.locked  
      view(Server) !! commit()
       await msg commitAck from all(Server)   
  }
  use msg prepareAck of core
   handle msg prepareAck from all(Server) where some ~(prepareAck.
  locked)  {
      let lockOK = {m.sender: m \in {m \in all(prepareAck): m.locked}}
      lockOK !! release()
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
  state Nil<BOOLEAN> pvalue = NIL
   handle msg prepare   {
    | pvalue = NIL ->  change pvalue = prepare.val
                       prepare.sender ! prepareAck(locked = TRUE)
    | pvalue # NIL ->  prepare.sender ! prepareAck(locked = FALSE)
  }
  using [selective_receive] handle msg commit   {
      assert (pvalue # NIL,"Commit is missing previous prepare.")
      change cvalue = pvalue,pvalue = NIL
      commit.sender ! commitAck()
  }
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
  }
  use state pvalue of core
   handle msg release   {
      change pvalue = NIL
  }
}
--------

-------- AFTER rewriteStateInit -------------------
CONST  NIL,init_Server
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
 tla {   rdVal(rdResSet) ==
           any2(rdResSet,"val") }
 tla {   Consistency ==
           \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).
                                 pvalue = NIL
                              THEN (st_Server[a]).cvalue = (st_Server[b]).
                                   cvalue
                              ELSE TRUE }
msg Client -> Set<Server> prepare(BOOLEAN val)
msg Server -> Client prepareAck(BOOLEAN locked)
msg Client -> Set<Server> commit()
msg Server -> Client commitAck()
msg Client -> Set<Server> rd()
msg Server -> Client rdRes(BOOLEAN val)
msg Client -> Set<Server> release()

display swimlane {
  msg prepare color = blue
  msg prepareAck color = IF locked THEN green ELSE red
  msg prepareAck style = IF locked THEN solid ELSE dashed
}
display swimlane {
  msg release color = yellow
}
role Client {
  state views Set<Server> = init_Server
  handle event rdwr(BOOLEAN v)  {
      view(Server) !! rd()
       await msg rdRes from all(Server)   
      let newVal = ~(rdVal(rdRes))
      view(Server) !! prepare(val = newVal)
       await msg prepareAck from all(Server) where all prepareAck.locked  
      view(Server) !! commit()
       await msg commitAck from all(Server)   
  }
  use msg prepareAck of core
   handle msg prepareAck from all(Server) where some ~(prepareAck.
  locked)  {
      let lockOK = {m.sender: m \in {m \in all(prepareAck): m.locked}}
      lockOK !! release()
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
  state Nil<BOOLEAN> pvalue = NIL
   handle msg prepare   {
    | pvalue = NIL ->  change pvalue = prepare.val
                       prepare.sender ! prepareAck(locked = TRUE)
    | pvalue # NIL ->  prepare.sender ! prepareAck(locked = FALSE)
  }
  using [selective_receive] handle msg commit   {
      assert (pvalue # NIL,"Commit is missing previous prepare.")
      change cvalue = pvalue,pvalue = NIL
      commit.sender ! commitAck()
  }
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
  }
  use state pvalue of core
   handle msg release   {
      change pvalue = NIL
  }
}
--------

-------- AFTER rewriteDoMeanwhile -----------------
CONST  NIL,init_Server
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
 tla {   rdVal(rdResSet) ==
           any2(rdResSet,"val") }
 tla {   Consistency ==
           \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).
                                 pvalue = NIL
                              THEN (st_Server[a]).cvalue = (st_Server[b]).
                                   cvalue
                              ELSE TRUE }
msg Client -> Set<Server> prepare(BOOLEAN val)
msg Server -> Client prepareAck(BOOLEAN locked)
msg Client -> Set<Server> commit()
msg Server -> Client commitAck()
msg Client -> Set<Server> rd()
msg Server -> Client rdRes(BOOLEAN val)
msg Client -> Set<Server> release()

display swimlane {
  msg prepare color = blue
  msg prepareAck color = IF locked THEN green ELSE red
  msg prepareAck style = IF locked THEN solid ELSE dashed
}
display swimlane {
  msg release color = yellow
}
role Client {
  state views Set<Server> = init_Server
  handle event rdwr(BOOLEAN v)  {
      view(Server) !! rd()
       await msg rdRes from all(Server)   
      let newVal = ~(rdVal(rdRes))
      view(Server) !! prepare(val = newVal)
       await msg prepareAck from all(Server) where all prepareAck.locked  
      view(Server) !! commit()
       await msg commitAck from all(Server)   
  }
  use msg prepareAck of core
   handle msg prepareAck from all(Server) where some ~(prepareAck.
  locked)  {
      let lockOK = {m.sender: m \in {m \in all(prepareAck): m.locked}}
      lockOK !! release()
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
  state Nil<BOOLEAN> pvalue = NIL
   handle msg prepare   {
    | pvalue = NIL ->  change pvalue = prepare.val
                       prepare.sender ! prepareAck(locked = TRUE)
    | pvalue # NIL ->  prepare.sender ! prepareAck(locked = FALSE)
  }
  using [selective_receive] handle msg commit   {
      assert (pvalue # NIL,"Commit is missing previous prepare.")
      change cvalue = pvalue,pvalue = NIL
      commit.sender ! commitAck()
  }
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
  }
  use state pvalue of core
   handle msg release   {
      change pvalue = NIL
  }
}
--------

-------- AFTER rewriteCont ------------------------
CONST  NIL,init_Server
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
 tla {   rdVal(rdResSet) ==
           any2(rdResSet,"val") }
 tla {   Consistency ==
           \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).
                                 pvalue = NIL
                              THEN (st_Server[a]).cvalue = (st_Server[b]).
                                   cvalue
                              ELSE TRUE }
msg Client -> Set<Server> prepare(BOOLEAN val)
msg Server -> Client prepareAck(BOOLEAN locked)
msg Client -> Set<Server> commit()
msg Server -> Client commitAck()
msg Client -> Set<Server> rd()
msg Server -> Client rdRes(BOOLEAN val)
msg Client -> Set<Server> release()

display swimlane {
  msg prepare color = blue
  msg prepareAck color = IF locked THEN green ELSE red
  msg prepareAck style = IF locked THEN solid ELSE dashed
}
display swimlane {
  msg release color = yellow
}
role Client {
  state views Set<Server> = init_Server
  use msg prepareAck of core
   handle msg prepareAck from all(Server) where some ~(prepareAck.
  locked)  {
      let lockOK = {m.sender: m \in {m \in all(prepareAck): m.locked}}
      lockOK !! release()
  }
  when g_pc_rdwr = 0 handle event rdwr(BOOLEAN v)  {
      view(Server) !! rd()
      change g_pc_rdwr = 1
  }
  state (0..3) g_pc_rdwr = 0
   when g_pc_rdwr = 1 handles msg rdRes from all(Server)   {
      let newVal = ~(rdVal(rdRes))
      view(Server) !! prepare(val = newVal)
      change g_pc_rdwr = 2
  }
   when g_pc_rdwr = 2 handles msg prepareAck from all
  (Server) where all prepareAck.locked  {
      view(Server) !! commit()
      change g_pc_rdwr = 3
  }
   when g_pc_rdwr = 3 handles msg commitAck from all(Server)   {
      change g_pc_rdwr = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
  state Nil<BOOLEAN> pvalue = NIL
   handle msg prepare   {
    | pvalue = NIL ->  change pvalue = prepare.val
                       prepare.sender ! prepareAck(locked = TRUE)
    | pvalue # NIL ->  prepare.sender ! prepareAck(locked = FALSE)
  }
  using [selective_receive] handle msg commit   {
      assert (pvalue # NIL,"Commit is missing previous prepare.")
      change cvalue = pvalue,pvalue = NIL
      commit.sender ! commitAck()
  }
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
  }
  use state pvalue of core
   handle msg release   {
      change pvalue = NIL
  }
}
--------

-------- AFTER rewriteONCE ------------------------
CONST  NIL,init_Server
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
 tla {   rdVal(rdResSet) ==
           any2(rdResSet,"val") }
 tla {   Consistency ==
           \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).
                                 pvalue = NIL
                              THEN (st_Server[a]).cvalue = (st_Server[b]).
                                   cvalue
                              ELSE TRUE }
msg Client -> Set<Server> prepare(BOOLEAN val)
msg Server -> Client prepareAck(BOOLEAN locked)
msg Client -> Set<Server> commit()
msg Server -> Client commitAck()
msg Client -> Set<Server> rd()
msg Server -> Client rdRes(BOOLEAN val)
msg Client -> Set<Server> release()

display swimlane {
  msg prepare color = blue
  msg prepareAck color = IF locked THEN green ELSE red
  msg prepareAck style = IF locked THEN solid ELSE dashed
}
display swimlane {
  msg release color = yellow
}
role Client {
  state views Set<Server> = init_Server
  use msg prepareAck of core
   handle msg prepareAck from all(Server) where some ~(prepareAck.
  locked)  {
      let lockOK = {m.sender: m \in {m \in all(prepareAck): m.locked}}
      lockOK !! release()
  }
  when g_pc_rdwr = 0 handle event rdwr(BOOLEAN v)  {
      view(Server) !! rd()
      change g_pc_rdwr = 1
  }
  state (0..3) g_pc_rdwr = 0
   when g_pc_rdwr = 1 handles msg rdRes from all(Server)   {
      let newVal = ~(rdVal(rdRes))
      view(Server) !! prepare(val = newVal)
      change g_pc_rdwr = 2
  }
   when g_pc_rdwr = 2 handles msg prepareAck from all
  (Server) where all prepareAck.locked  {
      view(Server) !! commit()
      change g_pc_rdwr = 3
  }
   when g_pc_rdwr = 3 handles msg commitAck from all(Server)   {
      change g_pc_rdwr = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
  state Nil<BOOLEAN> pvalue = NIL
   handle msg prepare   {
    | pvalue = NIL ->  change pvalue = prepare.val
                       prepare.sender ! prepareAck(locked = TRUE)
    | pvalue # NIL ->  prepare.sender ! prepareAck(locked = FALSE)
  }
  using [selective_receive] handle msg commit   {
      assert (pvalue # NIL,"Commit is missing previous prepare.")
      change cvalue = pvalue,pvalue = NIL
      commit.sender ! commitAck()
  }
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
  }
  use state pvalue of core
   handle msg release   {
      change pvalue = NIL
  }
}
--------

-------- AFTER rewriteTimer -----------------------
CONST  NIL,init_Server
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
 tla {   rdVal(rdResSet) ==
           any2(rdResSet,"val") }
 tla {   Consistency ==
           \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).
                                 pvalue = NIL
                              THEN (st_Server[a]).cvalue = (st_Server[b]).
                                   cvalue
                              ELSE TRUE }
msg Client -> Set<Server> prepare(BOOLEAN val)
msg Server -> Client prepareAck(BOOLEAN locked)
msg Client -> Set<Server> commit()
msg Server -> Client commitAck()
msg Client -> Set<Server> rd()
msg Server -> Client rdRes(BOOLEAN val)
msg Client -> Set<Server> release()

display swimlane {
  msg prepare color = blue
  msg prepareAck color = IF locked THEN green ELSE red
  msg prepareAck style = IF locked THEN solid ELSE dashed
}
display swimlane {
  msg release color = yellow
}
role Client {
  state views Set<Server> = init_Server
  use msg prepareAck of core
   handle msg prepareAck from all(Server) where some ~(prepareAck.
  locked)  {
      let lockOK = {m.sender: m \in {m \in all(prepareAck): m.locked}}
      lockOK !! release()
  }
  when g_pc_rdwr = 0 handle event rdwr(BOOLEAN v)  {
      view(Server) !! rd()
      change g_pc_rdwr = 1
  }
  state (0..3) g_pc_rdwr = 0
   when g_pc_rdwr = 1 handles msg rdRes from all(Server)   {
      let newVal = ~(rdVal(rdRes))
      view(Server) !! prepare(val = newVal)
      change g_pc_rdwr = 2
  }
   when g_pc_rdwr = 2 handles msg prepareAck from all
  (Server) where all prepareAck.locked  {
      view(Server) !! commit()
      change g_pc_rdwr = 3
  }
   when g_pc_rdwr = 3 handles msg commitAck from all(Server)   {
      change g_pc_rdwr = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
  state Nil<BOOLEAN> pvalue = NIL
   handle msg prepare   {
    | pvalue = NIL ->  change pvalue = prepare.val
                       prepare.sender ! prepareAck(locked = TRUE)
    | pvalue # NIL ->  prepare.sender ! prepareAck(locked = FALSE)
  }
  using [selective_receive] handle msg commit   {
      assert (pvalue # NIL,"Commit is missing previous prepare.")
      change cvalue = pvalue,pvalue = NIL
      commit.sender ! commitAck()
  }
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
  }
  use state pvalue of core
   handle msg release   {
      change pvalue = NIL
  }
}
--------

-------- AFTER RewriteMsgSetHandler ---------------
CONST  NIL,init_Server
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
 tla {   rdVal(rdResSet) ==
           any2(rdResSet,"val") }
 tla {   Consistency ==
           \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).
                                 pvalue = NIL
                              THEN (st_Server[a]).cvalue = (st_Server[b]).
                                   cvalue
                              ELSE TRUE }
msg Client -> Set<Server> prepare(BOOLEAN val)
msg Server -> Client prepareAck(BOOLEAN locked)
msg Client -> Set<Server> commit()
msg Server -> Client commitAck()
msg Client -> Set<Server> rd()
msg Server -> Client rdRes(BOOLEAN val)
msg Client -> Set<Server> release()

display swimlane {
  msg prepare color = blue
  msg prepareAck color = IF locked THEN green ELSE red
  msg prepareAck style = IF locked THEN solid ELSE dashed
}
display swimlane {
  msg release color = yellow
}
role Client {
  state views Set<Server> = init_Server
  use msg prepareAck of core
   handle msg prepareAck from all(Server) where some ~(prepareAck.
  locked)  {
      let lockOK = {m.sender: m \in {m \in all(prepareAck): m.locked}}
      lockOK !! release()
  }
  when g_pc_rdwr = 0 handle event rdwr(BOOLEAN v)  {
      view(Server) !! rd()
      change g_pc_rdwr = 1
  }
  state (0..3) g_pc_rdwr = 0
   when g_pc_rdwr = 1 handles msg rdRes from all(Server)   {
      let newVal = ~(rdVal(rdRes))
      view(Server) !! prepare(val = newVal)
      change g_pc_rdwr = 2
  }
   when g_pc_rdwr = 2 handles msg prepareAck from all
  (Server) where all prepareAck.locked  {
      view(Server) !! commit()
      change g_pc_rdwr = 3
  }
   when g_pc_rdwr = 3 handles msg commitAck from all(Server)   {
      change g_pc_rdwr = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
  state Nil<BOOLEAN> pvalue = NIL
   handle msg prepare   {
    | pvalue = NIL ->  change pvalue = prepare.val
                       prepare.sender ! prepareAck(locked = TRUE)
    | pvalue # NIL ->  prepare.sender ! prepareAck(locked = FALSE)
  }
  using [selective_receive] handle msg commit   {
      assert (pvalue # NIL,"Commit is missing previous prepare.")
      change cvalue = pvalue,pvalue = NIL
      commit.sender ! commitAck()
  }
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
  }
  use state pvalue of core
   handle msg release   {
      change pvalue = NIL
  }
}
--------

-------- AFTER rewriteLifecycle -------------------
CONST  NIL,init_Server,InitDownClient,CrashClient,StartClient,InitDownServer,CrashServer,StartServer
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
 tla {   rdVal(rdResSet) ==
           any2(rdResSet,"val") }
 tla {   Consistency ==
           \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).
                                 pvalue = NIL
                              THEN (st_Server[a]).cvalue = (st_Server[b]).
                                   cvalue
                              ELSE TRUE }
msg Client -> Set<Server> prepare(BOOLEAN val)
msg Server -> Client prepareAck(BOOLEAN locked)
msg Client -> Set<Server> commit()
msg Server -> Client commitAck()
msg Client -> Set<Server> rd()
msg Server -> Client rdRes(BOOLEAN val)
msg Client -> Set<Server> release()

display swimlane {
  msg prepare color = blue
  msg prepareAck color = IF locked THEN green ELSE red
  msg prepareAck style = IF locked THEN solid ELSE dashed
}
display swimlane {
  msg release color = yellow
}
role Client {
  state views Set<Server> = init_Server
  use msg prepareAck of core
   handle msg prepareAck from all(Server) where some ~(prepareAck.
  locked)  {
      let lockOK = {m.sender: m \in {m \in all(prepareAck): m.locked}}
      lockOK !! release()
  }
  when g_pc_rdwr = 0 handle event rdwr(BOOLEAN v)  {
      view(Server) !! rd()
      change g_pc_rdwr = 1
  }
  state (0..3) g_pc_rdwr = 0
   when g_pc_rdwr = 1 handles msg rdRes from all(Server)   {
      let newVal = ~(rdVal(rdRes))
      view(Server) !! prepare(val = newVal)
      change g_pc_rdwr = 2
  }
   when g_pc_rdwr = 2 handles msg prepareAck from all
  (Server) where all prepareAck.locked  {
      view(Server) !! commit()
      change g_pc_rdwr = 3
  }
   when g_pc_rdwr = 3 handles msg commitAck from all(Server)   {
      change g_pc_rdwr = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
  state Nil<BOOLEAN> pvalue = NIL
   handle msg prepare   {
    | pvalue = NIL ->  change pvalue = prepare.val
                       prepare.sender ! prepareAck(locked = TRUE)
    | pvalue # NIL ->  prepare.sender ! prepareAck(locked = FALSE)
  }
  using [selective_receive] handle msg commit   {
      assert (pvalue # NIL,"Commit is missing previous prepare.")
      change cvalue = pvalue,pvalue = NIL
      commit.sender ! commitAck()
  }
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
  }
  use state pvalue of core
   handle msg release   {
      change pvalue = NIL
  }
}
--------

-------- AFTER groupSendInstr ---------------------
CONST  NIL,init_Server,InitDownClient,CrashClient,StartClient,InitDownServer,CrashServer,StartServer
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
 tla {   rdVal(rdResSet) ==
           any2(rdResSet,"val") }
 tla {   Consistency ==
           \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).
                                 pvalue = NIL
                              THEN (st_Server[a]).cvalue = (st_Server[b]).
                                   cvalue
                              ELSE TRUE }
msg Client -> Set<Server> prepare(BOOLEAN val)
msg Server -> Client prepareAck(BOOLEAN locked)
msg Client -> Set<Server> commit()
msg Server -> Client commitAck()
msg Client -> Set<Server> rd()
msg Server -> Client rdRes(BOOLEAN val)
msg Client -> Set<Server> release()

display swimlane {
  msg prepare color = blue
  msg prepareAck color = IF locked THEN green ELSE red
  msg prepareAck style = IF locked THEN solid ELSE dashed
}
display swimlane {
  msg release color = yellow
}
role Client {
  state views Set<Server> = init_Server
  use msg prepareAck of core
   handle msg prepareAck from all(Server) where some ~(prepareAck.
  locked)  {
      let lockOK = {m.sender: m \in {m \in all(prepareAck): m.locked}}
      send-group lockOK !! release()
  }
  when g_pc_rdwr = 0 handle event rdwr(BOOLEAN v)  {
      change g_pc_rdwr = 1
      send-group view(Server) !! rd()
  }
  state (0..3) g_pc_rdwr = 0
   when g_pc_rdwr = 1 handles msg rdRes from all(Server)   {
      let newVal = ~(rdVal(rdRes))
      change g_pc_rdwr = 2
      send-group view(Server) !! prepare(val = newVal)
  }
   when g_pc_rdwr = 2 handles msg prepareAck from all
  (Server) where all prepareAck.locked  {
      change g_pc_rdwr = 3
      send-group view(Server) !! commit()
  }
   when g_pc_rdwr = 3 handles msg commitAck from all(Server)   {
      change g_pc_rdwr = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
  state Nil<BOOLEAN> pvalue = NIL
   handle msg prepare   {
    | pvalue = NIL ->  change pvalue = prepare.val
                       send-group prepare.sender ! prepareAck(locked = TRUE)
    | pvalue # NIL ->  send-group prepare.sender ! prepareAck
                                  (locked = FALSE)
  }
  using [selective_receive] handle msg commit   {
      assert (pvalue # NIL,"Commit is missing previous prepare.")
      change cvalue = pvalue,pvalue = NIL
      send-group commit.sender ! commitAck()
  }
   handle msg rd   {
      send-group rd.sender ! rdRes(val = cvalue)
  }
  use state pvalue of core
   handle msg release   {
      change pvalue = NIL
  }
}
--------

-------- AFTER RewriteExtendHook ------------------
CONST  NIL,init_Server,InitDownClient,CrashClient,StartClient,InitDownServer,CrashServer,StartServer
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
 tla {   rdVal(rdResSet) ==
           any2(rdResSet,"val") }
 tla {   Consistency ==
           \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).
                                 pvalue = NIL
                              THEN (st_Server[a]).cvalue = (st_Server[b]).
                                   cvalue
                              ELSE TRUE }
msg Client -> Set<Server> prepare(BOOLEAN val)
msg Server -> Client prepareAck(BOOLEAN locked)
msg Client -> Set<Server> commit()
msg Server -> Client commitAck()
msg Client -> Set<Server> rd()
msg Server -> Client rdRes(BOOLEAN val)
msg Client -> Set<Server> release()

display swimlane {
  msg prepare color = blue
  msg prepareAck color = IF locked THEN green ELSE red
  msg prepareAck style = IF locked THEN solid ELSE dashed
}
display swimlane {
  msg release color = yellow
}
role Client {
  state views Set<Server> = init_Server
  use msg prepareAck of core
   handle msg prepareAck from all(Server) where some ~(prepareAck.
  locked)  {
      let lockOK = {m.sender: m \in {m \in all(prepareAck): m.locked}}
      send-group lockOK !! release()
  }
  when g_pc_rdwr = 0 handle event rdwr(BOOLEAN v)  {
      change g_pc_rdwr = 1
      send-group view(Server) !! rd()
  }
  state (0..3) g_pc_rdwr = 0
   when g_pc_rdwr = 1 handles msg rdRes from all(Server)   {
      let newVal = ~(rdVal(rdRes))
      change g_pc_rdwr = 2
      send-group view(Server) !! prepare(val = newVal)
  }
   when g_pc_rdwr = 2 handles msg prepareAck from all
  (Server) where all prepareAck.locked  {
      change g_pc_rdwr = 3
      send-group view(Server) !! commit()
  }
   when g_pc_rdwr = 3 handles msg commitAck from all(Server)   {
      change g_pc_rdwr = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
  state Nil<BOOLEAN> pvalue = NIL
   handle msg prepare   {
    | pvalue = NIL ->  change pvalue = prepare.val
                       send-group prepare.sender ! prepareAck(locked = TRUE)
    | pvalue # NIL ->  send-group prepare.sender ! prepareAck
                                  (locked = FALSE)
  }
  using [selective_receive] handle msg commit   {
      assert (pvalue # NIL,"Commit is missing previous prepare.")
      change cvalue = pvalue,pvalue = NIL
      send-group commit.sender ! commitAck()
  }
   handle msg rd   {
      send-group rd.sender ! rdRes(val = cvalue)
  }
  use state pvalue of core
   handle msg release   {
      change pvalue = NIL
  }
}
--------

-------- AFTER Rewrite special operators ----------
CONST  NIL,init_Server,InitDownClient,CrashClient,StartClient,InitDownServer,CrashServer,StartServer
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
 tla {   rdVal(rdResSet) ==
           ANY2(rdResSet,"val") }
 tla {   Consistency ==
           \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).
                                 pvalue = NIL
                              THEN (st_Server[a]).cvalue = (st_Server[b]).
                                   cvalue
                              ELSE TRUE }
msg Client -> Set<Server> prepare(BOOLEAN val)
msg Server -> Client prepareAck(BOOLEAN locked)
msg Client -> Set<Server> commit()
msg Server -> Client commitAck()
msg Client -> Set<Server> rd()
msg Server -> Client rdRes(BOOLEAN val)
msg Client -> Set<Server> release()

display swimlane {
  msg prepare color = blue
  msg prepareAck color = IF locked THEN green ELSE red
  msg prepareAck style = IF locked THEN solid ELSE dashed
}
display swimlane {
  msg release color = yellow
}
role Client {
  state views Set<Server> = init_Server
  use msg prepareAck of core
   handle msg prepareAck from all(Server) where some ~(prepareAck.
  locked)  {
      let lockOK = {m.sender: m \in {m \in ALL(prepareAck): m.locked}}
      send-group lockOK !! release()
  }
  when g_pc_rdwr = 0 handle event rdwr(BOOLEAN v)  {
      change g_pc_rdwr = 1
      send-group view(Server) !! rd()
  }
  state (0..3) g_pc_rdwr = 0
   when g_pc_rdwr = 1 handles msg rdRes from all(Server)   {
      let newVal = ~(rdVal(rdRes))
      change g_pc_rdwr = 2
      send-group view(Server) !! prepare(val = newVal)
  }
   when g_pc_rdwr = 2 handles msg prepareAck from all
  (Server) where all prepareAck.locked  {
      change g_pc_rdwr = 3
      send-group view(Server) !! commit()
  }
   when g_pc_rdwr = 3 handles msg commitAck from all(Server)   {
      change g_pc_rdwr = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
  state Nil<BOOLEAN> pvalue = NIL
   handle msg prepare   {
    | pvalue = NIL ->  change pvalue = prepare.val
                       send-group prepare.sender ! prepareAck(locked = TRUE)
    | pvalue # NIL ->  send-group prepare.sender ! prepareAck
                                  (locked = FALSE)
  }
  using [selective_receive] handle msg commit   {
      assert (pvalue # NIL,"Commit is missing previous prepare.")
      change cvalue = pvalue,pvalue = NIL
      send-group commit.sender ! commitAck()
  }
   handle msg rd   {
      send-group rd.sender ! rdRes(val = cvalue)
  }
  use state pvalue of core
   handle msg release   {
      change pvalue = NIL
  }
}
--------

-------- AFTER RewriteOverrideTLA ----------
CONST  NIL,init_Server,InitDownClient,CrashClient,StartClient,InitDownServer,CrashServer,StartServer
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
 tla {   rdVal(rdResSet) ==
           ANY2(rdResSet,"val") }
 tla {   Consistency ==
           \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).
                                 pvalue = NIL
                              THEN (st_Server[a]).cvalue = (st_Server[b]).
                                   cvalue
                              ELSE TRUE }
msg Client -> Set<Server> prepare(BOOLEAN val)
msg Server -> Client prepareAck(BOOLEAN locked)
msg Client -> Set<Server> commit()
msg Server -> Client commitAck()
msg Client -> Set<Server> rd()
msg Server -> Client rdRes(BOOLEAN val)
msg Client -> Set<Server> release()

display swimlane {
  msg prepare color = blue
  msg prepareAck color = IF locked THEN green ELSE red
  msg prepareAck style = IF locked THEN solid ELSE dashed
}
display swimlane {
  msg release color = yellow
}
role Client {
  state views Set<Server> = init_Server
  use msg prepareAck of core
   handle msg prepareAck from all(Server) where some ~(prepareAck.
  locked)  {
      let lockOK = {m.sender: m \in {m \in ALL(prepareAck): m.locked}}
      send-group lockOK !! release()
  }
  when g_pc_rdwr = 0 handle event rdwr(BOOLEAN v)  {
      change g_pc_rdwr = 1
      send-group view(Server) !! rd()
  }
  state (0..3) g_pc_rdwr = 0
   when g_pc_rdwr = 1 handles msg rdRes from all(Server)   {
      let newVal = ~(rdVal(rdRes))
      change g_pc_rdwr = 2
      send-group view(Server) !! prepare(val = newVal)
  }
   when g_pc_rdwr = 2 handles msg prepareAck from all
  (Server) where all prepareAck.locked  {
      change g_pc_rdwr = 3
      send-group view(Server) !! commit()
  }
   when g_pc_rdwr = 3 handles msg commitAck from all(Server)   {
      change g_pc_rdwr = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
  state Nil<BOOLEAN> pvalue = NIL
   handle msg prepare   {
    | pvalue = NIL ->  change pvalue = prepare.val
                       send-group prepare.sender ! prepareAck(locked = TRUE)
    | pvalue # NIL ->  send-group prepare.sender ! prepareAck
                                  (locked = FALSE)
  }
  using [selective_receive] handle msg commit   {
      assert (pvalue # NIL,"Commit is missing previous prepare.")
      change cvalue = pvalue,pvalue = NIL
      send-group commit.sender ! commitAck()
  }
   handle msg rd   {
      send-group rd.sender ! rdRes(val = cvalue)
  }
  use state pvalue of core
   handle msg release   {
      change pvalue = NIL
  }
}
--------


---- MODULE lu ----
EXTENDS TLC,Naturals,FiniteSets,Sequences
---- 
CONSTANT NIL,init_Server,InitDownClient,CrashClient,StartClient,InitDownServer,CrashServer,StartServer,PrepareAck_4242(_,_),RdRes_4242(_,_),CommitAck_4242(_,_),Prepare_4242(_,_),Commit_4242(_,_),Rd_4242(_,_),Release_4242(_,_),Client,Server 
VARIABLE st_Client,st_Server 
---- 
ClientMultiSendToServer(client) ==
  /\ (Len((st_Client[client]).g_obuf) > 0 /\ LET e ==
                                                   Head((st_Client[client]).g_obuf) 
                                                 m ==
                                                   e[1] 
                                                 d ==
                                                   e[2]
                                              IN IF d = {}
                                                 THEN /\ (st_Client' = [st_Client EXCEPT ![client].g_obuf = Tail(@)] /\ UNCHANGED (<<st_Server>>))
                                                 ELSE \E p \in d: /\ (p \in Server /\ IF Cardinality(d) = 1 THEN /\ (st_Client' = [st_Client EXCEPT ![client].g_obuf = Tail(@)] /\ st_Server' = [st_Server EXCEPT ![p].g_inbox = Append(@,m)]) ELSE /\ (st_Client' = [st_Client EXCEPT ![client].g_obuf = [@ EXCEPT ![1] = <<m,d \ {p}>>]] /\ st_Server' = [st_Server EXCEPT ![p].g_inbox = Append(@,m)]))) 
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
rdVal(rdResSet) ==
  ANY2(rdResSet,"val") 
Consistency ==
  \A a,b \in Server: IF (st_Server[a]).pvalue = NIL /\ (st_Server[b]).pvalue = NIL THEN (st_Server[a]).cvalue = (st_Server[b]).cvalue ELSE TRUE 
---- 
Msg ==
  [type: {"prepare"},sender: Client,val: BOOLEAN] \cup [type: {"prepareAck"},sender: Server,locked: BOOLEAN] \cup [type: {"commit"},sender: Client] \cup [type: {"commitAck"},sender: Server] \cup [type: {"rd"},sender: Client] \cup [type: {"rdRes"},sender: Server,val: BOOLEAN] \cup [type: {"release"},sender: Client] 
---- 
ClientState ==
  [g_pc_rdwr: (0..3),g_view_Server: SUBSET (Server),g_inbox: Seq(Msg),g_obuf: Seq(Msg \X (SUBSET (Server)))] 
ServerState ==
  [cvalue: BOOLEAN,pvalue: BOOLEAN \cup {NIL},g_inbox: Seq(Msg)] 
---- 
TypeInvariant ==
  /\ (st_Client \in [Client -> ClientState]) /\ (st_Server \in [Server -> ServerState]) 
Init ==
  /\ (st_Client = [client \in Client |-> [g_pc_rdwr |-> 0,g_view_Server |-> init_Server,g_inbox |-> <<>>,g_obuf |-> <<>>]]) /\ (st_Server = [server \in Server |-> [cvalue |-> FALSE,pvalue |-> NIL,g_inbox |-> <<>>]]) 
---- 
ZzZPrepareAck_4242(client,msgpos) ==
  /\ (Len((st_Client[client]).g_inbox) > 0)
  /\ (LET is ==
            {local_i \in DOMAIN((st_Client[client]).g_inbox): /\ (((st_Client[client]).g_inbox[local_i]).type = "prepareAck") /\ (((st_Client[client]).g_inbox[local_i]).sender \in (st_Client[client]).g_view_Server)} 
          maxi ==
            IF is # {} THEN CHOOSE local_i \in is: \A local_j \in is: local_j <= local_i ELSE 0 
          prepareAck ==
            {(st_Client[client]).g_inbox[local_i]: local_i \in is} 
          sender_set ==
            {m.sender: m \in prepareAck} 
          KeepMsgTest(m) ==
            m \notin prepareAck
       IN /\ (/\ (sender_set = (st_Client[client]).g_view_Server) /\ ({} # (st_Client[client]).g_view_Server))
          /\ (IF msgpos = 0 THEN /\ (\A local_i \in 1..maxi: TRUE) /\ (\E temp_m \in prepareAck: ~(temp_m.locked)) /\ (LET lockOK ==   {m.sender: m \in {m \in ALL(prepareAck): m.locked}}  IN /\ (st_Client' = [st_Client EXCEPT ![client].g_obuf = Append(@,<<[type |-> "release",sender |-> client],lockOK>>),![client].g_inbox = SelectSeq((st_Client[client]).g_inbox,KeepMsgTest)]) /\ (UNCHANGED (<<st_Server>>))) ELSE maxi # 0 /\ maxi <= msgpos)) 
Rdwr_4242(client,v) ==
  /\ ((st_Client[client]).g_pc_rdwr = 0) /\ (/\ (st_Client' = [st_Client EXCEPT ![client].g_pc_rdwr = 1,![client].g_obuf = Append(@,<<[type |-> "rd",sender |-> client],(st_Client[client]).g_view_Server>>)]) /\ (UNCHANGED (<<st_Server>>))) 
ZzZRdRes_4242(client,msgpos) ==
  /\ ((st_Client[client]).g_pc_rdwr = 1)
  /\ (Len((st_Client[client]).g_inbox) > 0)
  /\ (LET is ==
            {local_i \in DOMAIN((st_Client[client]).g_inbox): /\ (((st_Client[client]).g_inbox[local_i]).type = "rdRes") /\ (((st_Client[client]).g_inbox[local_i]).sender \in (st_Client[client]).g_view_Server)} 
          maxi ==
            IF is # {} THEN CHOOSE local_i \in is: \A local_j \in is: local_j <= local_i ELSE 0 
          rdRes ==
            {(st_Client[client]).g_inbox[local_i]: local_i \in is} 
          sender_set ==
            {m.sender: m \in rdRes} 
          KeepMsgTest(m) ==
            m \notin rdRes
       IN /\ (/\ (sender_set = (st_Client[client]).g_view_Server) /\ ({} # (st_Client[client]).g_view_Server))
          /\ (IF msgpos = 0 THEN /\ (\A local_i \in 1..maxi: TRUE) /\ (LET newVal ==   ~(rdVal(rdRes))  IN /\ (st_Client' = [st_Client EXCEPT ![client].g_pc_rdwr = 2,![client].g_obuf = Append(@,<<[type |-> "prepare",sender |-> client,val |-> newVal],(st_Client[client]).g_view_Server>>),![client].g_inbox = SelectSeq((st_Client[client]).g_inbox,KeepMsgTest)]) /\ (UNCHANGED (<<st_Server>>))) ELSE maxi # 0 /\ maxi <= msgpos)) 
ZzZCommitAck_4242(client,msgpos) ==
  /\ ((st_Client[client]).g_pc_rdwr = 3)
  /\ (Len((st_Client[client]).g_inbox) > 0)
  /\ (LET is ==
            {local_i \in DOMAIN((st_Client[client]).g_inbox): /\ (((st_Client[client]).g_inbox[local_i]).type = "commitAck") /\ (((st_Client[client]).g_inbox[local_i]).sender \in (st_Client[client]).g_view_Server)} 
          maxi ==
            IF is # {} THEN CHOOSE local_i \in is: \A local_j \in is: local_j <= local_i ELSE 0 
          commitAck ==
            {(st_Client[client]).g_inbox[local_i]: local_i \in is} 
          sender_set ==
            {m.sender: m \in commitAck} 
          KeepMsgTest(m) ==
            m \notin commitAck
       IN /\ (/\ (sender_set = (st_Client[client]).g_view_Server) /\ ({} # (st_Client[client]).g_view_Server)) /\ (IF msgpos = 0 THEN /\ (\A local_i \in 1..maxi: TRUE) /\ (/\ (st_Client' = [st_Client EXCEPT ![client].g_pc_rdwr = 0,![client].g_inbox = SelectSeq((st_Client[client]).g_inbox,KeepMsgTest)]) /\ (UNCHANGED (<<st_Server>>))) ELSE maxi # 0 /\ maxi <= msgpos)) 
ZzZPrepare_4242(server,msgpos) ==
  /\ (Len((st_Server[server]).g_inbox) > 0)
  /\ (LET Local_F(local_server,prepare) ==
            \/ ((st_Server[local_server]).pvalue = NIL) \/ ((st_Server[local_server]).pvalue # NIL) 
          local_p ==
            IF Head((st_Server[server]).g_inbox).type = "prepare" THEN 1 ELSE 0
       IN /\ (local_p > 0)
          /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos)
          /\ (LET prepare ==
                    (st_Server[server]).g_inbox[local_p]
               IN CASE (st_Server[server]).pvalue = NIL -> /\ (st_Server' = [st_Server EXCEPT ![server].pvalue = prepare.val,![server].g_inbox = DropPos((st_Server[server]).g_inbox,local_p)]) /\ (st_Client' = [st_Client EXCEPT ![prepare.sender].g_inbox = Append(@,[type |-> "prepareAck",sender |-> server,locked |-> TRUE])]) [] 
                       (st_Server[server]).pvalue # NIL -> /\ (st_Client' = [st_Client EXCEPT ![prepare.sender].g_inbox = Append(@,[type |-> "prepareAck",sender |-> server,locked |-> FALSE])]) /\ (st_Server' = [st_Server EXCEPT ![server].g_inbox = DropPos((st_Server[server]).g_inbox,local_p)]))) 
ZzZCommit_4242(server,msgpos) ==
  /\ (Len((st_Server[server]).g_inbox) > 0)
  /\ (LET Local_F(local_server,commit) ==
            /\ (TRUE) /\ (TRUE) /\ (TRUE) 
          local_p ==
            MsgPos((st_Server[server]).g_inbox,"commit",Local_F,server)
       IN /\ (local_p > 0)
          /\ (IF msgpos = 0 THEN \A local_i \in 1..local_p-1: /\ (~(ENABLED(Prepare_4242(server,local_i)))) /\ (~(ENABLED(Rd_4242(server,local_i)))) /\ (~(ENABLED(Release_4242(server,local_i)))) ELSE local_p = msgpos)
          /\ (LET commit ==
                    (st_Server[server]).g_inbox[local_p]
               IN /\ (Assert(\/ (msgpos # 0) \/ ((st_Server[server]).pvalue # NIL),<<"Commit is missing previous prepare.">>)) /\ (st_Server' = [st_Server EXCEPT ![server].cvalue = (st_Server[server]).pvalue,![server].pvalue = NIL,![server].g_inbox = DropPos((st_Server[server]).g_inbox,local_p)]) /\ (st_Client' = [st_Client EXCEPT ![commit.sender].g_inbox = Append(@,[type |-> "commitAck",sender |-> server])]))) 
ZzZRd_4242(server,msgpos) ==
  /\ (Len((st_Server[server]).g_inbox) > 0)
  /\ (LET Local_F(local_server,rd) ==
            /\ (TRUE) /\ (TRUE) 
          local_p ==
            IF Head((st_Server[server]).g_inbox).type = "rd" THEN 1 ELSE 0
       IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET rd ==   (st_Server[server]).g_inbox[local_p]  IN /\ (st_Client' = [st_Client EXCEPT ![rd.sender].g_inbox = Append(@,[type |-> "rdRes",sender |-> server,val |-> (st_Server[server]).cvalue])]) /\ (st_Server' = [st_Server EXCEPT ![server].g_inbox = DropPos((st_Server[server]).g_inbox,local_p)]))) 
ZzZRelease_4242(server,msgpos) ==
  /\ (Len((st_Server[server]).g_inbox) > 0)
  /\ (LET Local_F(local_server,release) ==   /\ (TRUE) /\ (TRUE) local_p ==   IF Head((st_Server[server]).g_inbox).type = "release" THEN 1 ELSE 0  IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET release ==   (st_Server[server]).g_inbox[local_p]  IN /\ (st_Server' = [st_Server EXCEPT ![server].pvalue = NIL,![server].g_inbox = DropPos((st_Server[server]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_Client>>)))) 
---- 
Fairness ==
  /\ (WF_<<st_Client,st_Server>>(\E client \in Client: PrepareAck_4242(client,0)))
  /\ (WF_<<st_Client,st_Server>>(\E client \in Client,v \in BOOLEAN: Rdwr_4242(client,v)))
  /\ (WF_<<st_Client,st_Server>>(\E client \in Client: RdRes_4242(client,0)))
  /\ (WF_<<st_Client,st_Server>>(\E client \in Client: PrepareAck_4242(client,0)))
  /\ (WF_<<st_Client,st_Server>>(\E client \in Client: CommitAck_4242(client,0)))
  /\ (WF_<<st_Client,st_Server>>(\E server \in Server: Prepare_4242(server,0)))
  /\ (WF_<<st_Client,st_Server>>(\E server \in Server: Commit_4242(server,0)))
  /\ (WF_<<st_Client,st_Server>>(\E server \in Server: Rd_4242(server,0)))
  /\ (WF_<<st_Client,st_Server>>(\E server \in Server: Release_4242(server,0)))
  /\ (WF_<<st_Client,st_Server>>(\E client \in Client: ClientMultiSendToServer(client)))
  /\ (WF_<<st_Client,st_Server>>(\E client \in Client: ClientMultiSendToServer(client)))
  /\ (WF_<<st_Client,st_Server>>(\E client \in Client: ClientMultiSendToServer(client)))
  /\ (WF_<<st_Client,st_Server>>(\E client \in Client: ClientMultiSendToServer(client))) 
---- 
Next ==
  \/ (\E client \in Client: PrepareAck_4242(client,0))
  \/ (\E client \in Client,v \in BOOLEAN: Rdwr_4242(client,v))
  \/ (\E client \in Client: RdRes_4242(client,0))
  \/ (\E client \in Client: PrepareAck_4242(client,0))
  \/ (\E client \in Client: CommitAck_4242(client,0))
  \/ (\E server \in Server: Prepare_4242(server,0))
  \/ (\E server \in Server: Commit_4242(server,0))
  \/ (\E server \in Server: Rd_4242(server,0))
  \/ (\E server \in Server: Release_4242(server,0))
  \/ (\E client \in Client: ClientMultiSendToServer(client))
  \/ (\E client \in Client: ClientMultiSendToServer(client))
  \/ (\E client \in Client: ClientMultiSendToServer(client))
  \/ (\E client \in Client: ClientMultiSendToServer(client)) 
---- 
Spec ==
  /\ (Init) /\ ([]([Next]_<<st_Client,st_Server>>)) /\ (Fairness)
====
