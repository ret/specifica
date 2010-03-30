---- PARTS ----
protocol prim_singlemsg_rcv concern default {
  roles Sender,Receiver
  interaction foo {
    msg Sender -> Receiver a(BOOLEAN enab)
    role Sender {
      handles once go  {
          let r = CHOOSE r \in Receiver: TRUE
          r ! a(enab = FALSE)
          r ! a(enab = TRUE)
      }
    }
    role Receiver {
      state BOOLEAN s = TRUE
      state BOOLEAN ok = FALSE
      using [selective_receive] handle msg a   {
        | s /\ a.enab ->  change ok = TRUE
        | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
      }
      when ok handle event ok()  {
          assert (FALSE,"Good, did receive!",self)
      }
    }
  }
  interaction x {
    msg Sender -> Set<Receiver> foo()
    role Sender {
      when FALSE handle event dummy()  {
          Receiver !! foo()
      }
    }
  }
}
--------


-------- AFTER Merging interactions ---------------
protocol prim_singlemsg_rcv concern __generated {
  roles Sender,Receiver
  interaction __generated {
    msg Sender -> Receiver a(BOOLEAN enab)
    msg Sender -> Set<Receiver> foo()
    role Sender {
      handles once go  {
          let r = CHOOSE r \in Receiver: TRUE
          r ! a(enab = FALSE)
          r ! a(enab = TRUE)
      }
      when FALSE handle event dummy()  {
          Receiver !! foo()
      }
    }
    role Receiver {
      state BOOLEAN s = TRUE
      state BOOLEAN ok = FALSE
      using [selective_receive] handle msg a   {
        | s /\ a.enab ->  change ok = TRUE
        | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
      }
      when ok handle event ok()  {
          assert (FALSE,"Good, did receive!",self)
      }
    }
  }
}
--------

-------- AFTER insideOut (flatten) ----------------
 

msg Sender -> Receiver a(BOOLEAN enab)
msg Sender -> Set<Receiver> foo()


role Sender {
  handles once go  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a(enab = FALSE)
      r ! a(enab = TRUE)
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
}
role Receiver {
  state BOOLEAN s = TRUE
  state BOOLEAN ok = FALSE
  using [selective_receive] handle msg a   {
    | s /\ a.enab ->  change ok = TRUE
    | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
  }
  when ok handle event ok()  {
      assert (FALSE,"Good, did receive!",self)
  }
}
--------

-------- AFTER rewriteWhen ------------------------
 

msg Sender -> Receiver a(BOOLEAN enab)
msg Sender -> Set<Receiver> foo()


role Sender {
  handles once go  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a(enab = FALSE)
      r ! a(enab = TRUE)
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
}
role Receiver {
  state BOOLEAN s = TRUE
  state BOOLEAN ok = FALSE
  using [selective_receive] handle msg a   {
    | s /\ a.enab ->  change ok = TRUE
    | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
  }
  when ok handle event ok()  {
      assert (FALSE,"Good, did receive!",self)
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
msg Sender -> Receiver a(BOOLEAN enab)
msg Sender -> Set<Receiver> foo()


role Sender {
  handles once go  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a(enab = FALSE)
      r ! a(enab = TRUE)
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
}
role Receiver {
  state BOOLEAN s = TRUE
  state BOOLEAN ok = FALSE
  using [selective_receive] handle msg a   {
    | s /\ a.enab ->  change ok = TRUE
    | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
  }
  when ok handle event ok()  {
      assert (FALSE,"Good, did receive!",self)
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
msg Sender -> Receiver a(BOOLEAN enab)
msg Sender -> Set<Receiver> foo()


role Sender {
  handles once go  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a(enab = FALSE)
      r ! a(enab = TRUE)
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
}
role Receiver {
  state BOOLEAN s = TRUE
  state BOOLEAN ok = FALSE
  using [selective_receive] handle msg a   {
    | s /\ a.enab ->  change ok = TRUE
    | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
  }
  when ok handle event ok()  {
      assert (FALSE,"Good, did receive!",self)
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
msg Sender -> Receiver a(BOOLEAN enab)
msg Sender -> Set<Receiver> foo()


role Sender {
  handles once go  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a(enab = FALSE)
      r ! a(enab = TRUE)
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
}
role Receiver {
  state BOOLEAN s = TRUE
  state BOOLEAN ok = FALSE
  using [selective_receive] handle msg a   {
    | s /\ a.enab ->  change ok = TRUE
    | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
  }
  when ok handle event ok()  {
      assert (FALSE,"Good, did receive!",self)
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
msg Sender -> Receiver a(BOOLEAN enab)
msg Sender -> Set<Receiver> foo()


role Sender {
  handles once go  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a(enab = FALSE)
      r ! a(enab = TRUE)
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
}
role Receiver {
  state BOOLEAN s = TRUE
  state BOOLEAN ok = FALSE
  using [selective_receive] handle msg a   {
    | s /\ a.enab ->  change ok = TRUE
    | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
  }
  when ok handle event ok()  {
      assert (FALSE,"Good, did receive!",self)
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
msg Sender -> Receiver a(BOOLEAN enab)
msg Sender -> Set<Receiver> foo()


role Sender {
  when /\ (~(g_once_go)) handle event g_once_go()  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a(enab = FALSE)
      r ! a(enab = TRUE)
      change g_once_go = TRUE
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
}
role Receiver {
  state BOOLEAN s = TRUE
  state BOOLEAN ok = FALSE
  using [selective_receive] handle msg a   {
    | s /\ a.enab ->  change ok = TRUE
    | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
  }
  when ok handle event ok()  {
      assert (FALSE,"Good, did receive!",self)
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
msg Sender -> Receiver a(BOOLEAN enab)
msg Sender -> Set<Receiver> foo()


role Sender {
  when /\ (~(g_once_go)) handle event g_once_go()  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a(enab = FALSE)
      r ! a(enab = TRUE)
      change g_once_go = TRUE
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
}
role Receiver {
  state BOOLEAN s = TRUE
  state BOOLEAN ok = FALSE
  using [selective_receive] handle msg a   {
    | s /\ a.enab ->  change ok = TRUE
    | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
  }
  when ok handle event ok()  {
      assert (FALSE,"Good, did receive!",self)
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
msg Sender -> Receiver a(BOOLEAN enab)
msg Sender -> Set<Receiver> foo()


role Sender {
  when /\ (~(g_once_go)) handle event g_once_go()  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a(enab = FALSE)
      r ! a(enab = TRUE)
      change g_once_go = TRUE
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
}
role Receiver {
  state BOOLEAN s = TRUE
  state BOOLEAN ok = FALSE
  using [selective_receive] handle msg a   {
    | s /\ a.enab ->  change ok = TRUE
    | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
  }
  when ok handle event ok()  {
      assert (FALSE,"Good, did receive!",self)
  }
}
--------

-------- AFTER rewriteLifecycle -------------------
CONST  InitDownSender,CrashSender,StartSender,InitDownReceiver,CrashReceiver,StartReceiver
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
msg Sender -> Receiver a(BOOLEAN enab)
msg Sender -> Set<Receiver> foo()


role Sender {
  when /\ (~(g_once_go)) handle event g_once_go()  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a(enab = FALSE)
      r ! a(enab = TRUE)
      change g_once_go = TRUE
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
}
role Receiver {
  state BOOLEAN s = TRUE
  state BOOLEAN ok = FALSE
  using [selective_receive] handle msg a   {
    | s /\ a.enab ->  change ok = TRUE
    | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
  }
  when ok handle event ok()  {
      assert (FALSE,"Good, did receive!",self)
  }
}
--------

-------- AFTER groupSendInstr ---------------------
CONST  InitDownSender,CrashSender,StartSender,InitDownReceiver,CrashReceiver,StartReceiver
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
msg Sender -> Receiver a(BOOLEAN enab)
msg Sender -> Set<Receiver> foo()


role Sender {
  when /\ (~(g_once_go)) handle event g_once_go()  {
      let r = CHOOSE r \in Receiver: TRUE
      change g_once_go = TRUE
      send-group r ! a(enab = FALSE)
                 r ! a(enab = TRUE)
  }
  when FALSE handle event dummy()  {
      send-group Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
}
role Receiver {
  state BOOLEAN s = TRUE
  state BOOLEAN ok = FALSE
  using [selective_receive] handle msg a   {
    | s /\ a.enab ->  change ok = TRUE
    | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
  }
  when ok handle event ok()  {
      assert (FALSE,"Good, did receive!",self)
  }
}
--------

-------- AFTER RewriteExtendHook ------------------
CONST  InitDownSender,CrashSender,StartSender,InitDownReceiver,CrashReceiver,StartReceiver
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
msg Sender -> Receiver a(BOOLEAN enab)
msg Sender -> Set<Receiver> foo()


role Sender {
  when /\ (~(g_once_go)) handle event g_once_go()  {
      let r = CHOOSE r \in Receiver: TRUE
      change g_once_go = TRUE
      send-group r ! a(enab = FALSE)
                 r ! a(enab = TRUE)
  }
  when FALSE handle event dummy()  {
      send-group Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
}
role Receiver {
  state BOOLEAN s = TRUE
  state BOOLEAN ok = FALSE
  using [selective_receive] handle msg a   {
    | s /\ a.enab ->  change ok = TRUE
    | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
  }
  when ok handle event ok()  {
      assert (FALSE,"Good, did receive!",self)
  }
}
--------

-------- AFTER Rewrite special operators ----------
CONST  InitDownSender,CrashSender,StartSender,InitDownReceiver,CrashReceiver,StartReceiver
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
msg Sender -> Receiver a(BOOLEAN enab)
msg Sender -> Set<Receiver> foo()


role Sender {
  when /\ (~(g_once_go)) handle event g_once_go()  {
      let r = CHOOSE r \in Receiver: TRUE
      change g_once_go = TRUE
      send-group r ! a(enab = FALSE)
                 r ! a(enab = TRUE)
  }
  when FALSE handle event dummy()  {
      send-group Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
}
role Receiver {
  state BOOLEAN s = TRUE
  state BOOLEAN ok = FALSE
  using [selective_receive] handle msg a   {
    | s /\ a.enab ->  change ok = TRUE
    | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
  }
  when ok handle event ok()  {
      assert (FALSE,"Good, did receive!",self)
  }
}
--------

-------- AFTER RewriteOverrideTLA ----------
CONST  InitDownSender,CrashSender,StartSender,InitDownReceiver,CrashReceiver,StartReceiver
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
msg Sender -> Receiver a(BOOLEAN enab)
msg Sender -> Set<Receiver> foo()


role Sender {
  when /\ (~(g_once_go)) handle event g_once_go()  {
      let r = CHOOSE r \in Receiver: TRUE
      change g_once_go = TRUE
      send-group r ! a(enab = FALSE)
                 r ! a(enab = TRUE)
  }
  when FALSE handle event dummy()  {
      send-group Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
}
role Receiver {
  state BOOLEAN s = TRUE
  state BOOLEAN ok = FALSE
  using [selective_receive] handle msg a   {
    | s /\ a.enab ->  change ok = TRUE
    | otherwise ->  assert (FALSE,"Error,this should not trigger",self,a)
  }
  when ok handle event ok()  {
      assert (FALSE,"Good, did receive!",self)
  }
}
--------


---- MODULE prim_singlemsg_rcv ----
EXTENDS TLC,Naturals,FiniteSets,Sequences
---- 
CONSTANT InitDownSender,CrashSender,StartSender,InitDownReceiver,CrashReceiver,StartReceiver,A_4242(_,_),Sender,Receiver 
VARIABLE st_Sender,st_Receiver 
---- 
SenderMultiSendToReceiver(sender) ==
  /\ (Len((st_Sender[sender]).g_obuf) > 0 /\ LET e ==
                                                   Head((st_Sender[sender]).g_obuf) 
                                                 m ==
                                                   e[1] 
                                                 d ==
                                                   e[2]
                                              IN IF d = {}
                                                 THEN /\ (st_Sender' = [st_Sender EXCEPT ![sender].g_obuf = Tail(@)] /\ UNCHANGED (<<st_Receiver>>))
                                                 ELSE \E p \in d: /\ (p \in Receiver /\ IF Cardinality(d) = 1 THEN /\ (st_Sender' = [st_Sender EXCEPT ![sender].g_obuf = Tail(@)] /\ st_Receiver' = [st_Receiver EXCEPT ![p].g_inbox = Append(@,m)]) ELSE /\ (st_Sender' = [st_Sender EXCEPT ![sender].g_obuf = [@ EXCEPT ![1] = <<m,d \ {p}>>]] /\ st_Receiver' = [st_Receiver EXCEPT ![p].g_inbox = Append(@,m)]))) 
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
  [type: {"a"},sender: Sender,enab: BOOLEAN] \cup [type: {"foo"},sender: Sender] 
---- 
SenderState ==
  [g_once_go: BOOLEAN,g_obuf: Seq(Msg \X (SUBSET (Receiver)))] 
ReceiverState ==
  [s: BOOLEAN,ok: BOOLEAN,g_inbox: Seq(Msg)] 
---- 
TypeInvariant ==
  /\ (st_Sender \in [Sender -> SenderState]) /\ (st_Receiver \in [Receiver -> ReceiverState]) 
Init ==
  /\ (st_Sender = [sender \in Sender |-> [g_once_go |-> FALSE,g_obuf |-> <<>>]]) /\ (st_Receiver = [receiver \in Receiver |-> [s |-> TRUE,ok |-> FALSE,g_inbox |-> <<>>]]) 
---- 
G_once_go_4242(sender) ==
  /\ (/\ (~((st_Sender[sender]).g_once_go))) /\ (LET r ==   CHOOSE r \in Receiver: TRUE  IN /\ (st_Sender' = [st_Sender EXCEPT ![sender].g_once_go = TRUE,![sender].g_obuf = @ \o <<<<[type |-> "a",sender |-> sender,enab |-> FALSE],{r}>>,<<[type |-> "a",sender |-> sender,enab |-> TRUE],{r}>>>>]) /\ (UNCHANGED (<<st_Receiver>>))) 
Dummy_4242(sender) ==
  /\ (FALSE) /\ (/\ (st_Sender' = [st_Sender EXCEPT ![sender].g_obuf = Append(@,<<[type |-> "foo",sender |-> sender],Receiver>>)]) /\ (UNCHANGED (<<st_Receiver>>))) 
ZzZA_4242(receiver,msgpos) ==
  /\ (Len((st_Receiver[receiver]).g_inbox) > 0)
  /\ (LET Local_F(local_receiver,a) ==
            \/ ((st_Receiver[local_receiver]).s /\ a.enab) 
          local_p ==
            MsgPos((st_Receiver[receiver]).g_inbox,"a",Local_F,receiver)
       IN /\ (local_p > 0)
          /\ (IF msgpos = 0 THEN \A local_i \in 1..local_p-1: TRUE ELSE local_p = msgpos)
          /\ (LET a ==
                    (st_Receiver[receiver]).g_inbox[local_p]
               IN CASE (st_Receiver[receiver]).s /\ a.enab -> /\ (st_Receiver' = [st_Receiver EXCEPT ![receiver].ok = TRUE,![receiver].g_inbox = DropPos((st_Receiver[receiver]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_Sender>>)) [] 
                       OTHER -> /\ (Assert(\/ (msgpos # 0) \/ (FALSE),<<"Error,this should not trigger",<<"self",receiver>>,<<"a",a>>>>)) /\ (st_Receiver' = [st_Receiver EXCEPT ![receiver].g_inbox = DropPos((st_Receiver[receiver]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_Sender>>)))) 
Ok_4242(receiver) ==
  /\ ((st_Receiver[receiver]).ok) /\ (/\ (Assert(FALSE,<<"Good, did receive!",<<"self",receiver>>>>)) /\ (UNCHANGED (<<st_Sender,st_Receiver>>))) 
---- 
Fairness ==
  /\ (WF_<<st_Sender,st_Receiver>>(\E sender \in Sender: G_once_go_4242(sender))) /\ (WF_<<st_Sender,st_Receiver>>(\E sender \in Sender: Dummy_4242(sender))) /\ (WF_<<st_Sender,st_Receiver>>(\E receiver \in Receiver: A_4242(receiver,0))) /\ (WF_<<st_Sender,st_Receiver>>(\E receiver \in Receiver: Ok_4242(receiver))) /\ (WF_<<st_Sender,st_Receiver>>(\E sender \in Sender: SenderMultiSendToReceiver(sender))) 
---- 
Next ==
  \/ (\E sender \in Sender: G_once_go_4242(sender)) \/ (\E sender \in Sender: Dummy_4242(sender)) \/ (\E receiver \in Receiver: A_4242(receiver,0)) \/ (\E receiver \in Receiver: Ok_4242(receiver)) \/ (\E sender \in Sender: SenderMultiSendToReceiver(sender)) 
---- 
Spec ==
  /\ (Init) /\ ([]([Next]_<<st_Sender,st_Receiver>>)) /\ (Fairness)
====
