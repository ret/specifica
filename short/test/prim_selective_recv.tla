---- PARTS ----
protocol prim_selective_recv concern default {
  roles Sender,Receiver
  interaction check_single_send {
    msg Sender -> Receiver a()
    msg Sender -> Receiver b()
    msg Sender -> Receiver c()
    msg Sender -> Receiver d()
    role Sender {
      handles once go  {
          let r = CHOOSE r \in Receiver: TRUE
          r ! a()
          r ! b()
          r ! c()
          r ! d()
      }
       handle msg foo   {
          drop foo
      }
    }
    role Receiver {
      state BOOLEAN b_received = FALSE
      state BOOLEAN c_received = FALSE
       when FALSE handles msg a   {
          assert (FALSE,"a received")
      }
      using [selective_receive] handle msg b   {
          change b_received = TRUE
      }
      using [selective_receive] handle msg c   {
          assert (b_received,"b should have been received first!")
          change c_received = TRUE
      }
      using [selective_receive] handle msg d   {
          assert (b_received /\ c_received,
          "b and c should have been received first!")
      }
      when c_received handle event ok()  {
          assert (FALSE,"selective receive works!")
      }
      state BOOLEAN bar
       handle crash Sender s  {
          change bar = FALSE
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
protocol prim_selective_recv concern __generated {
  roles Sender,Receiver
  interaction __generated {
    msg Sender -> Receiver a()
    msg Sender -> Receiver b()
    msg Sender -> Receiver c()
    msg Sender -> Receiver d()
    msg Sender -> Set<Receiver> foo()
    role Sender {
      handles once go  {
          let r = CHOOSE r \in Receiver: TRUE
          r ! a()
          r ! b()
          r ! c()
          r ! d()
      }
       handle msg foo   {
          drop foo
      }
      when FALSE handle event dummy()  {
          Receiver !! foo()
      }
    }
    role Receiver {
      state BOOLEAN b_received = FALSE
      state BOOLEAN c_received = FALSE
       when FALSE handles msg a   {
          assert (FALSE,"a received")
      }
      using [selective_receive] handle msg b   {
          change b_received = TRUE
      }
      using [selective_receive] handle msg c   {
          assert (b_received,"b should have been received first!")
          change c_received = TRUE
      }
      using [selective_receive] handle msg d   {
          assert (b_received /\ c_received,
          "b and c should have been received first!")
      }
      when c_received handle event ok()  {
          assert (FALSE,"selective receive works!")
      }
      state BOOLEAN bar
       handle crash Sender s  {
          change bar = FALSE
      }
    }
  }
}
--------

-------- AFTER insideOut (flatten) ----------------
 

msg Sender -> Receiver a()
msg Sender -> Receiver b()
msg Sender -> Receiver c()
msg Sender -> Receiver d()
msg Sender -> Set<Receiver> foo()


role Sender {
  handles once go  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a()
      r ! b()
      r ! c()
      r ! d()
  }
   handle msg foo   {
      drop foo
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
}
role Receiver {
  state BOOLEAN b_received = FALSE
  state BOOLEAN c_received = FALSE
   when FALSE handles msg a   {
      assert (FALSE,"a received")
  }
  using [selective_receive] handle msg b   {
      change b_received = TRUE
  }
  using [selective_receive] handle msg c   {
      assert (b_received,"b should have been received first!")
      change c_received = TRUE
  }
  using [selective_receive] handle msg d   {
      assert (b_received /\ c_received,
      "b and c should have been received first!")
  }
  when c_received handle event ok()  {
      assert (FALSE,"selective receive works!")
  }
  state BOOLEAN bar
   handle crash Sender s  {
      change bar = FALSE
  }
}
--------

-------- AFTER rewriteWhen ------------------------
 

msg Sender -> Receiver a()
msg Sender -> Receiver b()
msg Sender -> Receiver c()
msg Sender -> Receiver d()
msg Sender -> Set<Receiver> foo()


role Sender {
  handles once go  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a()
      r ! b()
      r ! c()
      r ! d()
  }
   handle msg foo   {
      drop foo
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
}
role Receiver {
  state BOOLEAN b_received = FALSE
  state BOOLEAN c_received = FALSE
   when FALSE handles msg a   {
      assert (FALSE,"a received")
  }
  using [selective_receive] handle msg b   {
      change b_received = TRUE
  }
  using [selective_receive] handle msg c   {
      assert (b_received,"b should have been received first!")
      change c_received = TRUE
  }
  using [selective_receive] handle msg d   {
      assert (b_received /\ c_received,
      "b and c should have been received first!")
  }
  when c_received handle event ok()  {
      assert (FALSE,"selective receive works!")
  }
  state BOOLEAN bar
   handle crash Sender s  {
      change bar = FALSE
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
msg Sender -> Receiver a()
msg Sender -> Receiver b()
msg Sender -> Receiver c()
msg Sender -> Receiver d()
msg Sender -> Set<Receiver> foo()


role Sender {
  handles once go  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a()
      r ! b()
      r ! c()
      r ! d()
  }
   handle msg foo   {
      drop foo
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
}
role Receiver {
  state BOOLEAN b_received = FALSE
  state BOOLEAN c_received = FALSE
   when FALSE handles msg a   {
      assert (FALSE,"a received")
  }
  using [selective_receive] handle msg b   {
      change b_received = TRUE
  }
  using [selective_receive] handle msg c   {
      assert (b_received,"b should have been received first!")
      change c_received = TRUE
  }
  using [selective_receive] handle msg d   {
      assert (b_received /\ c_received,
      "b and c should have been received first!")
  }
  when c_received handle event ok()  {
      assert (FALSE,"selective receive works!")
  }
  state BOOLEAN bar
   handle crash Sender s  {
      change bar = FALSE
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
msg Sender -> Receiver a()
msg Sender -> Receiver b()
msg Sender -> Receiver c()
msg Sender -> Receiver d()
msg Sender -> Set<Receiver> foo()


role Sender {
  handles once go  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a()
      r ! b()
      r ! c()
      r ! d()
  }
   handle msg foo   {
      drop foo
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
}
role Receiver {
  state BOOLEAN b_received = FALSE
  state BOOLEAN c_received = FALSE
   when FALSE handles msg a   {
      assert (FALSE,"a received")
  }
  using [selective_receive] handle msg b   {
      change b_received = TRUE
  }
  using [selective_receive] handle msg c   {
      assert (b_received,"b should have been received first!")
      change c_received = TRUE
  }
  using [selective_receive] handle msg d   {
      assert (b_received /\ c_received,
      "b and c should have been received first!")
  }
  when c_received handle event ok()  {
      assert (FALSE,"selective receive works!")
  }
  state BOOLEAN bar = FALSE
   handle crash Sender s  {
      change bar = FALSE
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
msg Sender -> Receiver a()
msg Sender -> Receiver b()
msg Sender -> Receiver c()
msg Sender -> Receiver d()
msg Sender -> Set<Receiver> foo()


role Sender {
  handles once go  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a()
      r ! b()
      r ! c()
      r ! d()
  }
   handle msg foo   {
      drop foo
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
}
role Receiver {
  state BOOLEAN b_received = FALSE
  state BOOLEAN c_received = FALSE
   when FALSE handles msg a   {
      assert (FALSE,"a received")
  }
  using [selective_receive] handle msg b   {
      change b_received = TRUE
  }
  using [selective_receive] handle msg c   {
      assert (b_received,"b should have been received first!")
      change c_received = TRUE
  }
  using [selective_receive] handle msg d   {
      assert (b_received /\ c_received,
      "b and c should have been received first!")
  }
  when c_received handle event ok()  {
      assert (FALSE,"selective receive works!")
  }
  state BOOLEAN bar = FALSE
   handle crash Sender s  {
      change bar = FALSE
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
msg Sender -> Receiver a()
msg Sender -> Receiver b()
msg Sender -> Receiver c()
msg Sender -> Receiver d()
msg Sender -> Set<Receiver> foo()


role Sender {
  handles once go  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a()
      r ! b()
      r ! c()
      r ! d()
  }
   handle msg foo   {
      drop foo
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
}
role Receiver {
  state BOOLEAN b_received = FALSE
  state BOOLEAN c_received = FALSE
   when FALSE handles msg a   {
      assert (FALSE,"a received")
  }
  using [selective_receive] handle msg b   {
      change b_received = TRUE
  }
  using [selective_receive] handle msg c   {
      assert (b_received,"b should have been received first!")
      change c_received = TRUE
  }
  using [selective_receive] handle msg d   {
      assert (b_received /\ c_received,
      "b and c should have been received first!")
  }
  when c_received handle event ok()  {
      assert (FALSE,"selective receive works!")
  }
  state BOOLEAN bar = FALSE
   handle crash Sender s  {
      change bar = FALSE
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
msg Sender -> Receiver a()
msg Sender -> Receiver b()
msg Sender -> Receiver c()
msg Sender -> Receiver d()
msg Sender -> Set<Receiver> foo()


role Sender {
  when /\ (~(g_once_go)) handle event g_once_go()  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a()
      r ! b()
      r ! c()
      r ! d()
      change g_once_go = TRUE
  }
   handle msg foo   {
      drop foo
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
}
role Receiver {
  state BOOLEAN b_received = FALSE
  state BOOLEAN c_received = FALSE
   when FALSE handles msg a   {
      assert (FALSE,"a received")
  }
  using [selective_receive] handle msg b   {
      change b_received = TRUE
  }
  using [selective_receive] handle msg c   {
      assert (b_received,"b should have been received first!")
      change c_received = TRUE
  }
  using [selective_receive] handle msg d   {
      assert (b_received /\ c_received,
      "b and c should have been received first!")
  }
  when c_received handle event ok()  {
      assert (FALSE,"selective receive works!")
  }
  state BOOLEAN bar = FALSE
   handle crash Sender s  {
      change bar = FALSE
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
msg Sender -> Receiver a()
msg Sender -> Receiver b()
msg Sender -> Receiver c()
msg Sender -> Receiver d()
msg Sender -> Set<Receiver> foo()


role Sender {
  when /\ (~(g_once_go)) handle event g_once_go()  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a()
      r ! b()
      r ! c()
      r ! d()
      change g_once_go = TRUE
  }
   handle msg foo   {
      drop foo
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
}
role Receiver {
  state BOOLEAN b_received = FALSE
  state BOOLEAN c_received = FALSE
   when FALSE handles msg a   {
      assert (FALSE,"a received")
  }
  using [selective_receive] handle msg b   {
      change b_received = TRUE
  }
  using [selective_receive] handle msg c   {
      assert (b_received,"b should have been received first!")
      change c_received = TRUE
  }
  using [selective_receive] handle msg d   {
      assert (b_received /\ c_received,
      "b and c should have been received first!")
  }
  when c_received handle event ok()  {
      assert (FALSE,"selective receive works!")
  }
  state BOOLEAN bar = FALSE
   handle crash Sender s  {
      change bar = FALSE
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
msg Sender -> Receiver a()
msg Sender -> Receiver b()
msg Sender -> Receiver c()
msg Sender -> Receiver d()
msg Sender -> Set<Receiver> foo()


role Sender {
  when /\ (~(g_once_go)) handle event g_once_go()  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a()
      r ! b()
      r ! c()
      r ! d()
      change g_once_go = TRUE
  }
   handle msg foo   {
      drop foo
  }
  when FALSE handle event dummy()  {
      Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
}
role Receiver {
  state BOOLEAN b_received = FALSE
  state BOOLEAN c_received = FALSE
   when FALSE handles msg a   {
      assert (FALSE,"a received")
  }
  using [selective_receive] handle msg b   {
      change b_received = TRUE
  }
  using [selective_receive] handle msg c   {
      assert (b_received,"b should have been received first!")
      change c_received = TRUE
  }
  using [selective_receive] handle msg d   {
      assert (b_received /\ c_received,
      "b and c should have been received first!")
  }
  when c_received handle event ok()  {
      assert (FALSE,"selective receive works!")
  }
  state BOOLEAN bar = FALSE
   handle crash Sender s  {
      change bar = FALSE
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
msg Sender -> Receiver a()
msg Sender -> Receiver b()
msg Sender -> Receiver c()
msg Sender -> Receiver d()
msg Sender -> Set<Receiver> foo()
msg Sender -> Set<Receiver> crashSenderReceiver()


role GLOBAL {
  when /\ (\A sender \in Sender: ~((st_Sender[sender]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role Sender {
  when /\ (g_running) /\ (/\ (~(g_once_go))) handle event g_once_go
  ()  {
      let r = CHOOSE r \in Receiver: TRUE
      r ! a()
      r ! b()
      r ! c()
      r ! d()
      change g_once_go = TRUE
  }
   when g_running handles msg foo   {
      drop foo
  }
  when /\ (g_running) /\ (FALSE) handle event dummy()  {
      Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
  extend @hook_do_crash_Sender(self) {
      change g_once_go = FALSE
  }
  state BOOLEAN g_running = ~(sender \in InitDownSender)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (sender \in CrashSender) handle event do_crash_Sender() @
  hook_do_crash_Sender
  (self) {
      change g_running = FALSE
      {local_receiver \in Receiver: (st_Receiver[local_receiver]).
                                    g_running} !! crashSenderReceiver()
      change g_lifecycle = @+1
  }
  when /\ (~(g_running))
       /\ (g_lifecycle < 2) handle event do_start_Sender() @
  hook_do_start_Sender
  (self) {
    | sender \in StartSender ->  change g_lifecycle = @+1
                                 change g_running = TRUE
                                 change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
role Receiver {
  state BOOLEAN b_received = FALSE
  state BOOLEAN c_received = FALSE
   when /\ (g_running) /\ (FALSE) handles msg a   {
      assert (FALSE,"a received")
  }
  using [selective_receive] when g_running handles msg b   {
      change b_received = TRUE
  }
  using [selective_receive] when g_running handles msg c   {
      assert (b_received,"b should have been received first!")
      change c_received = TRUE
  }
  using [selective_receive] when g_running handles msg d   {
      assert (b_received /\ c_received,
      "b and c should have been received first!")
  }
  when /\ (g_running) /\ (c_received) handle event ok()  {
      assert (FALSE,"selective receive works!")
  }
  state BOOLEAN bar = FALSE
   when g_running handles msg crashSenderReceiver   {
      let s = crashSenderReceiver.sender
      change bar = FALSE
  }
  state BOOLEAN g_running = ~(receiver \in InitDownReceiver)
  state {0,1,2} g_lifecycle = 0
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
msg Sender -> Receiver a()
msg Sender -> Receiver b()
msg Sender -> Receiver c()
msg Sender -> Receiver d()
msg Sender -> Set<Receiver> foo()
msg Sender -> Set<Receiver> crashSenderReceiver()


role GLOBAL {
  when /\ (\A sender \in Sender: ~((st_Sender[sender]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role Sender {
  when /\ (g_running) /\ (/\ (~(g_once_go))) handle event g_once_go
  ()  {
      let r = CHOOSE r \in Receiver: TRUE
      change g_once_go = TRUE
      send-group r ! a()
                 r ! b()
                 r ! c()
                 r ! d()
  }
   when g_running handles msg foo   {
      drop foo
  }
  when /\ (g_running) /\ (FALSE) handle event dummy()  {
      send-group Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
  extend @hook_do_crash_Sender(self) {
      change g_once_go = FALSE
  }
  state BOOLEAN g_running = ~(sender \in InitDownSender)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (sender \in CrashSender) handle event do_crash_Sender() @
  hook_do_crash_Sender
  (self) {
      change g_running = FALSE
      change g_lifecycle = @+1
      send-group {local_receiver \in Receiver: (st_Receiver[local_receiver]).
                                               g_running} !! crashSenderReceiver
                 ()
  }
  when /\ (~(g_running))
       /\ (g_lifecycle < 2) handle event do_start_Sender() @
  hook_do_start_Sender
  (self) {
    | sender \in StartSender ->  change g_lifecycle = @+1
                                 change g_running = TRUE
                                 change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
role Receiver {
  state BOOLEAN b_received = FALSE
  state BOOLEAN c_received = FALSE
   when /\ (g_running) /\ (FALSE) handles msg a   {
      assert (FALSE,"a received")
  }
  using [selective_receive] when g_running handles msg b   {
      change b_received = TRUE
  }
  using [selective_receive] when g_running handles msg c   {
      assert (b_received,"b should have been received first!")
      change c_received = TRUE
  }
  using [selective_receive] when g_running handles msg d   {
      assert (b_received /\ c_received,
      "b and c should have been received first!")
  }
  when /\ (g_running) /\ (c_received) handle event ok()  {
      assert (FALSE,"selective receive works!")
  }
  state BOOLEAN bar = FALSE
   when g_running handles msg crashSenderReceiver   {
      let s = crashSenderReceiver.sender
      change bar = FALSE
  }
  state BOOLEAN g_running = ~(receiver \in InitDownReceiver)
  state {0,1,2} g_lifecycle = 0
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
msg Sender -> Receiver a()
msg Sender -> Receiver b()
msg Sender -> Receiver c()
msg Sender -> Receiver d()
msg Sender -> Set<Receiver> foo()
msg Sender -> Set<Receiver> crashSenderReceiver()


role GLOBAL {
  when /\ (\A sender \in Sender: ~((st_Sender[sender]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role Sender {
  when /\ (g_running) /\ (~(g_once_go)) handle event g_once_go()  {
      let r = CHOOSE r \in Receiver: TRUE
      change g_once_go = TRUE
      send-group r ! a()
                 r ! b()
                 r ! c()
                 r ! d()
  }
   when g_running handles msg foo   {
      drop foo
  }
  when /\ (g_running) /\ (FALSE) handle event dummy()  {
      send-group Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
  state BOOLEAN g_running = ~(sender \in InitDownSender)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (sender \in CrashSender) handle event do_crash_Sender()  {
      change g_running = FALSE
      change g_lifecycle = @+1
      send-group {local_receiver \in Receiver: (st_Receiver[local_receiver]).
                                               g_running} !! crashSenderReceiver
                 ()
      change g_once_go = FALSE
  }
  when /\ (~(g_running))
       /\ (g_lifecycle < 2) handle event do_start_Sender()  {
    | sender \in StartSender ->  change g_lifecycle = @+1
                                 change g_running = TRUE
                                 change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
role Receiver {
  state BOOLEAN b_received = FALSE
  state BOOLEAN c_received = FALSE
   when /\ (g_running) /\ (FALSE) handles msg a   {
      assert (FALSE,"a received")
  }
  using [selective_receive] when g_running handles msg b   {
      change b_received = TRUE
  }
  using [selective_receive] when g_running handles msg c   {
      assert (b_received,"b should have been received first!")
      change c_received = TRUE
  }
  using [selective_receive] when g_running handles msg d   {
      assert (b_received /\ c_received,
      "b and c should have been received first!")
  }
  when /\ (g_running) /\ (c_received) handle event ok()  {
      assert (FALSE,"selective receive works!")
  }
  state BOOLEAN bar = FALSE
   when g_running handles msg crashSenderReceiver   {
      let s = crashSenderReceiver.sender
      change bar = FALSE
  }
  state BOOLEAN g_running = ~(receiver \in InitDownReceiver)
  state {0,1,2} g_lifecycle = 0
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
msg Sender -> Receiver a()
msg Sender -> Receiver b()
msg Sender -> Receiver c()
msg Sender -> Receiver d()
msg Sender -> Set<Receiver> foo()
msg Sender -> Set<Receiver> crashSenderReceiver()


role GLOBAL {
  when /\ (\A sender \in Sender: ~((st_Sender[sender]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role Sender {
  when /\ (g_running) /\ (~(g_once_go)) handle event g_once_go()  {
      let r = CHOOSE r \in Receiver: TRUE
      change g_once_go = TRUE
      send-group r ! a()
                 r ! b()
                 r ! c()
                 r ! d()
  }
   when g_running handles msg foo   {
      drop foo
  }
  when /\ (g_running) /\ (FALSE) handle event dummy()  {
      send-group Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
  state BOOLEAN g_running = ~(sender \in InitDownSender)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (sender \in CrashSender) handle event do_crash_Sender()  {
      change g_running = FALSE
      change g_lifecycle = @+1
      send-group {local_receiver \in Receiver: (st_Receiver[local_receiver]).
                                               g_running} !! crashSenderReceiver
                 ()
      change g_once_go = FALSE
  }
  when /\ (~(g_running))
       /\ (g_lifecycle < 2) handle event do_start_Sender()  {
    | sender \in StartSender ->  change g_lifecycle = @+1
                                 change g_running = TRUE
                                 change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
role Receiver {
  state BOOLEAN b_received = FALSE
  state BOOLEAN c_received = FALSE
   when /\ (g_running) /\ (FALSE) handles msg a   {
      assert (FALSE,"a received")
  }
  using [selective_receive] when g_running handles msg b   {
      change b_received = TRUE
  }
  using [selective_receive] when g_running handles msg c   {
      assert (b_received,"b should have been received first!")
      change c_received = TRUE
  }
  using [selective_receive] when g_running handles msg d   {
      assert (b_received /\ c_received,
      "b and c should have been received first!")
  }
  when /\ (g_running) /\ (c_received) handle event ok()  {
      assert (FALSE,"selective receive works!")
  }
  state BOOLEAN bar = FALSE
   when g_running handles msg crashSenderReceiver   {
      let s = crashSenderReceiver.sender
      change bar = FALSE
  }
  state BOOLEAN g_running = ~(receiver \in InitDownReceiver)
  state {0,1,2} g_lifecycle = 0
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
msg Sender -> Receiver a()
msg Sender -> Receiver b()
msg Sender -> Receiver c()
msg Sender -> Receiver d()
msg Sender -> Set<Receiver> foo()
msg Sender -> Set<Receiver> crashSenderReceiver()


role GLOBAL {
  when /\ (\A sender \in Sender: ~((st_Sender[sender]).
              g_running)) handle event StutterWhenNothingRunning()  {
    
  }
  state BOOLEAN g_dummy = FALSE
}
role Sender {
  when /\ (g_running) /\ (~(g_once_go)) handle event g_once_go()  {
      let r = CHOOSE r \in Receiver: TRUE
      change g_once_go = TRUE
      send-group r ! a()
                 r ! b()
                 r ! c()
                 r ! d()
  }
   when g_running handles msg foo   {
      drop foo
  }
  when /\ (g_running) /\ (FALSE) handle event dummy()  {
      send-group Receiver !! foo()
  }
  state BOOLEAN g_once_go = FALSE
  state BOOLEAN g_running = ~(sender \in InitDownSender)
  state {0,1,2} g_lifecycle = 0
  when /\ (g_running)
       /\ (g_lifecycle < 2)
       /\ (sender \in CrashSender) handle event do_crash_Sender()  {
      change g_running = FALSE
      change g_lifecycle = @+1
      send-group {local_receiver \in Receiver: (st_Receiver[local_receiver]).
                                               g_running} !! crashSenderReceiver
                 ()
      change g_once_go = FALSE
  }
  when /\ (~(g_running))
       /\ (g_lifecycle < 2) handle event do_start_Sender()  {
    | sender \in StartSender ->  change g_lifecycle = @+1
                                 change g_running = TRUE
                                 change g_inbox = <<>>
    | otherwise ->  failtlaclause
  }
}
role Receiver {
  state BOOLEAN b_received = FALSE
  state BOOLEAN c_received = FALSE
   when /\ (g_running) /\ (FALSE) handles msg a   {
      assert (FALSE,"a received")
  }
  using [selective_receive] when g_running handles msg b   {
      change b_received = TRUE
  }
  using [selective_receive] when g_running handles msg c   {
      assert (b_received,"b should have been received first!")
      change c_received = TRUE
  }
  using [selective_receive] when g_running handles msg d   {
      assert (b_received /\ c_received,
      "b and c should have been received first!")
  }
  when /\ (g_running) /\ (c_received) handle event ok()  {
      assert (FALSE,"selective receive works!")
  }
  state BOOLEAN bar = FALSE
   when g_running handles msg crashSenderReceiver   {
      let s = crashSenderReceiver.sender
      change bar = FALSE
  }
  state BOOLEAN g_running = ~(receiver \in InitDownReceiver)
  state {0,1,2} g_lifecycle = 0
}
--------


---- MODULE prim_selective_recv ----
EXTENDS TLC,Naturals,FiniteSets,Sequences
---- 
CONSTANT InitDownSender,CrashSender,StartSender,InitDownReceiver,CrashReceiver,StartReceiver,Foo_4242(_,_),A_4242(_,_),B_4242(_,_),C_4242(_,_),D_4242(_,_),CrashSenderReceiver_4242(_,_),Sender,Receiver 
VARIABLE st_GLOBAL,st_Sender,st_Receiver 
---- 
SenderMultiSendToReceiver(sender) ==
  /\ (Len((st_Sender[sender]).g_obuf) > 0 /\ LET e ==
                                                   Head((st_Sender[sender]).g_obuf) 
                                                 m ==
                                                   e[1] 
                                                 d ==
                                                   e[2]
                                              IN IF d = {}
                                                 THEN /\ (st_Sender' = [st_Sender EXCEPT ![sender].g_obuf = Tail(@)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_Receiver>>))
                                                 ELSE \E p \in d: /\ (p \in Receiver /\ IF Cardinality(d) = 1 THEN /\ (st_Sender' = [st_Sender EXCEPT ![sender].g_obuf = Tail(@)] /\ st_Receiver' = [st_Receiver EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>)) ELSE /\ (st_Sender' = [st_Sender EXCEPT ![sender].g_obuf = [@ EXCEPT ![1] = <<m,d \ {p}>>]] /\ st_Receiver' = [st_Receiver EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>)))) 
SenderMultiSendToReceiver(sender) ==
  /\ (Len((st_Sender[sender]).g_obuf) > 0 /\ LET e ==
                                                   Head((st_Sender[sender]).g_obuf) 
                                                 m ==
                                                   e[1] 
                                                 d ==
                                                   e[2]
                                              IN IF d = {}
                                                 THEN /\ (st_Sender' = [st_Sender EXCEPT ![sender].g_obuf = Tail(@)] /\ UNCHANGED (<<st_GLOBAL>>) /\ UNCHANGED (<<st_Receiver>>))
                                                 ELSE \E p \in d: /\ (p \in Receiver /\ IF Cardinality(d) = 1 THEN /\ (st_Sender' = [st_Sender EXCEPT ![sender].g_obuf = Tail(@)] /\ st_Receiver' = [st_Receiver EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>)) ELSE /\ (st_Sender' = [st_Sender EXCEPT ![sender].g_obuf = [@ EXCEPT ![1] = <<m,d \ {p}>>]] /\ st_Receiver' = [st_Receiver EXCEPT ![p].g_inbox = Append(@,m)] /\ UNCHANGED (<<st_GLOBAL>>)))) 
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
  [type: {"a"},sender: Sender] \cup [type: {"b"},sender: Sender] \cup [type: {"c"},sender: Sender] \cup [type: {"d"},sender: Sender] \cup [type: {"foo"},sender: Sender] \cup [type: {"crashSenderReceiver"},sender: Sender] 
---- 
GLOBALState ==
  [g_dummy: BOOLEAN] 
SenderState ==
  [g_once_go: BOOLEAN,g_running: BOOLEAN,g_lifecycle: {0,1,2},g_inbox: Seq(Msg),g_obuf: Seq(Msg \X (SUBSET (Receiver)))] 
ReceiverState ==
  [b_received: BOOLEAN,c_received: BOOLEAN,bar: BOOLEAN,g_running: BOOLEAN,g_lifecycle: {0,1,2},g_inbox: Seq(Msg)] 
---- 
TypeInvariant ==
  /\ (st_GLOBAL \in GLOBALState) /\ (st_Sender \in [Sender -> SenderState]) /\ (st_Receiver \in [Receiver -> ReceiverState]) 
Init ==
  /\ (st_GLOBAL = [g_dummy |-> FALSE]) /\ (st_Sender = [sender \in Sender |-> [g_once_go |-> FALSE,g_running |-> ~(sender \in InitDownSender),g_lifecycle |-> 0,g_inbox |-> <<>>,g_obuf |-> <<>>]]) /\ (st_Receiver = [receiver \in Receiver |-> [b_received |-> FALSE,c_received |-> FALSE,bar |-> FALSE,g_running |-> ~(receiver \in InitDownReceiver),g_lifecycle |-> 0,g_inbox |-> <<>>]]) 
---- 
StutterWhenNothingRunning_4242 ==
  /\ (/\ (\A sender \in Sender: ~((st_Sender[sender]).g_running))) /\ (/\ (UNCHANGED (<<st_GLOBAL,st_Sender,st_Receiver>>))) 
G_once_go_4242(sender) ==
  /\ (/\ ((st_Sender[sender]).g_running) /\ (~((st_Sender[sender]).g_once_go))) /\ (LET r ==   CHOOSE r \in Receiver: TRUE  IN /\ (st_Sender' = [st_Sender EXCEPT ![sender].g_once_go = TRUE,![sender].g_obuf = @ \o <<<<[type |-> "a",sender |-> sender],{r}>>,<<[type |-> "b",sender |-> sender],{r}>>,<<[type |-> "c",sender |-> sender],{r}>>,<<[type |-> "d",sender |-> sender],{r}>>>>]) /\ (UNCHANGED (<<st_GLOBAL,st_Receiver>>))) 
ZzZFoo_4242(sender,msgpos) ==
  /\ ((st_Sender[sender]).g_running)
  /\ (Len((st_Sender[sender]).g_inbox) > 0)
  /\ (LET Local_F(local_sender,foo) ==   /\ (TRUE) /\ (TRUE) /\ (TRUE) local_p ==   IF Head((st_Sender[sender]).g_inbox).type = "foo" THEN 1 ELSE 0  IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET foo ==   (st_Sender[sender]).g_inbox[local_p]  IN /\ (TRUE) /\ (st_Sender' = [st_Sender EXCEPT ![sender].g_inbox = DropPos((st_Sender[sender]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_GLOBAL,st_Receiver>>)))) 
Dummy_4242(sender) ==
  /\ (/\ ((st_Sender[sender]).g_running) /\ (FALSE)) /\ (/\ (st_Sender' = [st_Sender EXCEPT ![sender].g_obuf = Append(@,<<[type |-> "foo",sender |-> sender],Receiver>>)]) /\ (UNCHANGED (<<st_GLOBAL,st_Receiver>>))) 
Do_crash_Sender_4242(sender) ==
  /\ (/\ ((st_Sender[sender]).g_running) /\ ((st_Sender[sender]).g_lifecycle < 2) /\ (sender \in CrashSender)) /\ (/\ (st_Sender' = [st_Sender EXCEPT ![sender].g_running = FALSE,![sender].g_lifecycle = @+1,![sender].g_obuf = <<<<[type |-> "crashSenderReceiver",sender |-> sender],{local_receiver \in Receiver: (st_Receiver[local_receiver]).g_running}>>>>,![sender].g_once_go = FALSE]) /\ (UNCHANGED (<<st_GLOBAL,st_Receiver>>))) 
Do_start_Sender_4242(sender) ==
  /\ (/\ (~((st_Sender[sender]).g_running)) /\ ((st_Sender[sender]).g_lifecycle < 2)) /\ (CASE sender \in StartSender -> /\ (st_Sender' = [st_Sender EXCEPT ![sender].g_lifecycle = @+1,![sender].g_running = TRUE,![sender].g_inbox = <<>>]) /\ (UNCHANGED (<<st_GLOBAL,st_Receiver>>)) [] OTHER -> /\ (FALSE) /\ (UNCHANGED (<<st_GLOBAL,st_Sender,st_Receiver>>))) 
ZzZA_4242(receiver,msgpos) ==
  /\ (/\ ((st_Receiver[receiver]).g_running) /\ (FALSE))
  /\ (Len((st_Receiver[receiver]).g_inbox) > 0)
  /\ (LET Local_F(local_receiver,a) ==
            /\ (TRUE) /\ (TRUE) /\ (TRUE) 
          local_p ==
            IF Head((st_Receiver[receiver]).g_inbox).type = "a" THEN 1 ELSE 0
       IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET a ==   (st_Receiver[receiver]).g_inbox[local_p]  IN /\ (Assert(\/ (msgpos # 0) \/ (FALSE),<<"a received">>)) /\ (st_Receiver' = [st_Receiver EXCEPT ![receiver].g_inbox = DropPos((st_Receiver[receiver]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_GLOBAL,st_Sender>>)))) 
ZzZB_4242(receiver,msgpos) ==
  /\ ((st_Receiver[receiver]).g_running)
  /\ (Len((st_Receiver[receiver]).g_inbox) > 0)
  /\ (LET Local_F(local_receiver,b) ==
            /\ (TRUE) /\ (TRUE) 
          local_p ==
            MsgPos((st_Receiver[receiver]).g_inbox,"b",Local_F,receiver)
       IN /\ (local_p > 0)
          /\ (IF msgpos = 0 THEN \A local_i \in 1..local_p-1: /\ (~(ENABLED(A_4242(receiver,local_i)))) /\ (~(ENABLED(C_4242(receiver,local_i)))) /\ (~(ENABLED(D_4242(receiver,local_i)))) /\ (~(ENABLED(CrashSenderReceiver_4242(receiver,local_i)))) ELSE local_p = msgpos)
          /\ (LET b ==   (st_Receiver[receiver]).g_inbox[local_p]  IN /\ (st_Receiver' = [st_Receiver EXCEPT ![receiver].b_received = TRUE,![receiver].g_inbox = DropPos((st_Receiver[receiver]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_GLOBAL,st_Sender>>)))) 
ZzZC_4242(receiver,msgpos) ==
  /\ ((st_Receiver[receiver]).g_running)
  /\ (Len((st_Receiver[receiver]).g_inbox) > 0)
  /\ (LET Local_F(local_receiver,c) ==
            /\ (TRUE) /\ (TRUE) /\ (TRUE) 
          local_p ==
            MsgPos((st_Receiver[receiver]).g_inbox,"c",Local_F,receiver)
       IN /\ (local_p > 0)
          /\ (IF msgpos = 0 THEN \A local_i \in 1..local_p-1: /\ (~(ENABLED(A_4242(receiver,local_i)))) /\ (~(ENABLED(B_4242(receiver,local_i)))) /\ (~(ENABLED(D_4242(receiver,local_i)))) /\ (~(ENABLED(CrashSenderReceiver_4242(receiver,local_i)))) ELSE local_p = msgpos)
          /\ (LET c ==   (st_Receiver[receiver]).g_inbox[local_p]  IN /\ (Assert(\/ (msgpos # 0) \/ ((st_Receiver[receiver]).b_received),<<"b should have been received first!">>)) /\ (st_Receiver' = [st_Receiver EXCEPT ![receiver].c_received = TRUE,![receiver].g_inbox = DropPos((st_Receiver[receiver]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_GLOBAL,st_Sender>>)))) 
ZzZD_4242(receiver,msgpos) ==
  /\ ((st_Receiver[receiver]).g_running)
  /\ (Len((st_Receiver[receiver]).g_inbox) > 0)
  /\ (LET Local_F(local_receiver,d) ==
            /\ (TRUE) /\ (TRUE) /\ (TRUE) 
          local_p ==
            MsgPos((st_Receiver[receiver]).g_inbox,"d",Local_F,receiver)
       IN /\ (local_p > 0)
          /\ (IF msgpos = 0 THEN \A local_i \in 1..local_p-1: /\ (~(ENABLED(A_4242(receiver,local_i)))) /\ (~(ENABLED(B_4242(receiver,local_i)))) /\ (~(ENABLED(C_4242(receiver,local_i)))) /\ (~(ENABLED(CrashSenderReceiver_4242(receiver,local_i)))) ELSE local_p = msgpos)
          /\ (LET d ==   (st_Receiver[receiver]).g_inbox[local_p]  IN /\ (Assert(\/ (msgpos # 0) \/ ((st_Receiver[receiver]).b_received /\ (st_Receiver[receiver]).c_received),<<"b and c should have been received first!">>)) /\ (st_Receiver' = [st_Receiver EXCEPT ![receiver].g_inbox = DropPos((st_Receiver[receiver]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_GLOBAL,st_Sender>>)))) 
Ok_4242(receiver) ==
  /\ (/\ ((st_Receiver[receiver]).g_running) /\ ((st_Receiver[receiver]).c_received)) /\ (/\ (Assert(FALSE,<<"selective receive works!">>)) /\ (UNCHANGED (<<st_GLOBAL,st_Sender,st_Receiver>>))) 
ZzZCrashSenderReceiver_4242(receiver,msgpos) ==
  /\ ((st_Receiver[receiver]).g_running)
  /\ (Len((st_Receiver[receiver]).g_inbox) > 0)
  /\ (LET Local_F(local_receiver,crashSenderReceiver) ==
            LET s ==   crashSenderReceiver.sender  IN /\ (TRUE) /\ (TRUE) 
          local_p ==
            IF Head((st_Receiver[receiver]).g_inbox).type = "crashSenderReceiver" THEN 1 ELSE 0
       IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET crashSenderReceiver ==   (st_Receiver[receiver]).g_inbox[local_p]  IN LET s ==   crashSenderReceiver.sender  IN /\ (st_Receiver' = [st_Receiver EXCEPT ![receiver].bar = FALSE,![receiver].g_inbox = DropPos((st_Receiver[receiver]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_GLOBAL,st_Sender>>)))) 
---- 
Fairness ==
  /\ (WF_<<st_GLOBAL,st_Sender,st_Receiver>>(StutterWhenNothingRunning_4242))
  /\ (WF_<<st_GLOBAL,st_Sender,st_Receiver>>(\E sender \in Sender: G_once_go_4242(sender)))
  /\ (WF_<<st_GLOBAL,st_Sender,st_Receiver>>(\E sender \in Sender: Foo_4242(sender,0)))
  /\ (WF_<<st_GLOBAL,st_Sender,st_Receiver>>(\E sender \in Sender: Dummy_4242(sender)))
  /\ (WF_<<st_GLOBAL,st_Sender,st_Receiver>>(\E sender \in Sender: Do_crash_Sender_4242(sender)))
  /\ (WF_<<st_GLOBAL,st_Sender,st_Receiver>>(\E sender \in Sender: Do_start_Sender_4242(sender)))
  /\ (WF_<<st_GLOBAL,st_Sender,st_Receiver>>(\E receiver \in Receiver: A_4242(receiver,0)))
  /\ (WF_<<st_GLOBAL,st_Sender,st_Receiver>>(\E receiver \in Receiver: B_4242(receiver,0)))
  /\ (WF_<<st_GLOBAL,st_Sender,st_Receiver>>(\E receiver \in Receiver: C_4242(receiver,0)))
  /\ (WF_<<st_GLOBAL,st_Sender,st_Receiver>>(\E receiver \in Receiver: D_4242(receiver,0)))
  /\ (WF_<<st_GLOBAL,st_Sender,st_Receiver>>(\E receiver \in Receiver: Ok_4242(receiver)))
  /\ (WF_<<st_GLOBAL,st_Sender,st_Receiver>>(\E receiver \in Receiver: CrashSenderReceiver_4242(receiver,0)))
  /\ (WF_<<st_GLOBAL,st_Sender,st_Receiver>>(\E sender \in Sender: SenderMultiSendToReceiver(sender)))
  /\ (WF_<<st_GLOBAL,st_Sender,st_Receiver>>(\E sender \in Sender: SenderMultiSendToReceiver(sender))) 
---- 
Next ==
  \/ (StutterWhenNothingRunning_4242)
  \/ (\E sender \in Sender: G_once_go_4242(sender))
  \/ (\E sender \in Sender: Foo_4242(sender,0))
  \/ (\E sender \in Sender: Dummy_4242(sender))
  \/ (\E sender \in Sender: Do_crash_Sender_4242(sender))
  \/ (\E sender \in Sender: Do_start_Sender_4242(sender))
  \/ (\E receiver \in Receiver: A_4242(receiver,0))
  \/ (\E receiver \in Receiver: B_4242(receiver,0))
  \/ (\E receiver \in Receiver: C_4242(receiver,0))
  \/ (\E receiver \in Receiver: D_4242(receiver,0))
  \/ (\E receiver \in Receiver: Ok_4242(receiver))
  \/ (\E receiver \in Receiver: CrashSenderReceiver_4242(receiver,0))
  \/ (\E sender \in Sender: SenderMultiSendToReceiver(sender))
  \/ (\E sender \in Sender: SenderMultiSendToReceiver(sender)) 
---- 
Spec ==
  /\ (Init) /\ ([]([Next]_<<st_GLOBAL,st_Sender,st_Receiver>>)) /\ (Fairness)
====
