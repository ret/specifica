---- PARTS ----
protocol prim_all1 concern default {
  constant X,init_servers
  roles Client,Server
  interaction basic(Client,Server) {
    msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
    msg Server -> Client res()
    role Client(Server init_servers) {
      state views Set<Server> = init_servers
      state BOOLEAN sent = FALSE
      state BOOLEAN done = FALSE
      when ~(sent) handle event go()  {
          change sent = TRUE
          view(Server) !! req(view = VIEW(Server),x = done)
      }
       handle msg res from majority(Server)   {
          change done = TRUE
      }
       when done handles msg res   {
          drop res
      }
    }
    role Server {
       handle msg req   {
          reply res()
      }
    }
  }
}
--------


-------- AFTER Merging interactions ---------------
protocol prim_all1 concern __generated {
  constant X,init_servers
  roles Client,Server
  interaction __generated {
    msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
    msg Server -> Client res()
    role Client(Server init_servers) {
      state views Set<Server> = init_servers
      state BOOLEAN sent = FALSE
      state BOOLEAN done = FALSE
      when ~(sent) handle event go()  {
          change sent = TRUE
          view(Server) !! req(view = VIEW(Server),x = done)
      }
       handle msg res from majority(Server)   {
          change done = TRUE
      }
       when done handles msg res   {
          drop res
      }
    }
    role Server {
       handle msg req   {
          reply res()
      }
    }
  }
}
--------

-------- AFTER insideOut (flatten) ----------------
CONST  X,init_servers

msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
msg Server -> Client res()


role Client(Server init_servers) {
  state views Set<Server> = init_servers
  state BOOLEAN sent = FALSE
  state BOOLEAN done = FALSE
  when ~(sent) handle event go()  {
      change sent = TRUE
      view(Server) !! req(view = VIEW(Server),x = done)
  }
   handle msg res from majority(Server)   {
      change done = TRUE
  }
   when done handles msg res   {
      drop res
  }
}
role Server {
   handle msg req   {
      reply res()
  }
}
--------

-------- AFTER rewriteWhen ------------------------
CONST  X,init_servers

msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
msg Server -> Client res()


role Client(Server init_servers) {
  state views Set<Server> = init_servers
  state BOOLEAN sent = FALSE
  state BOOLEAN done = FALSE
  when ~(sent) handle event go()  {
      change sent = TRUE
      view(Server) !! req(view = VIEW(Server),x = done)
  }
   handle msg res from majority(Server)   {
      change done = TRUE
  }
   when done handles msg res   {
      drop res
  }
}
role Server {
   handle msg req   {
      reply res()
  }
}
--------

-------- AFTER rewriteTAG -------------------------
CONST  X,init_servers
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
msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
msg Server -> Client res()


role Client(Server init_servers) {
  state views Set<Server> = init_servers
  state BOOLEAN sent = FALSE
  state BOOLEAN done = FALSE
  when ~(sent) handle event go()  {
      change sent = TRUE
      view(Server) !! req(view = VIEW(Server),x = done)
  }
   handle msg res from majority(Server)   {
      change done = TRUE
  }
   when done handles msg res   {
      drop res
  }
}
role Server {
   handle msg req   {
      req.sender ! res()
  }
}
--------

-------- AFTER rewriteStateInit -------------------
CONST  X,init_servers
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
msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
msg Server -> Client res()


role Client(Server init_servers) {
  state views Set<Server> = init_servers
  state BOOLEAN sent = FALSE
  state BOOLEAN done = FALSE
  when ~(sent) handle event go()  {
      change sent = TRUE
      view(Server) !! req(view = VIEW(Server),x = done)
  }
   handle msg res from majority(Server)   {
      change done = TRUE
  }
   when done handles msg res   {
      drop res
  }
}
role Server {
   handle msg req   {
      req.sender ! res()
  }
}
--------

-------- AFTER rewriteDoMeanwhile -----------------
CONST  X,init_servers
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
msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
msg Server -> Client res()


role Client(Server init_servers) {
  state views Set<Server> = init_servers
  state BOOLEAN sent = FALSE
  state BOOLEAN done = FALSE
  when ~(sent) handle event go()  {
      change sent = TRUE
      view(Server) !! req(view = VIEW(Server),x = done)
  }
   handle msg res from majority(Server)   {
      change done = TRUE
  }
   when done handles msg res   {
      drop res
  }
}
role Server {
   handle msg req   {
      req.sender ! res()
  }
}
--------

-------- AFTER rewriteCont ------------------------
CONST  X,init_servers
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
msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
msg Server -> Client res()


role Client(Server init_servers) {
  state views Set<Server> = init_servers
  state BOOLEAN sent = FALSE
  state BOOLEAN done = FALSE
  when ~(sent) handle event go()  {
      change sent = TRUE
      view(Server) !! req(view = VIEW(Server),x = done)
  }
   handle msg res from majority(Server)   {
      change done = TRUE
  }
   when done handles msg res   {
      drop res
  }
}
role Server {
   handle msg req   {
      req.sender ! res()
  }
}
--------

-------- AFTER rewriteONCE ------------------------
CONST  X,init_servers
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
msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
msg Server -> Client res()


role Client(Server init_servers) {
  state views Set<Server> = init_servers
  state BOOLEAN sent = FALSE
  state BOOLEAN done = FALSE
  when ~(sent) handle event go()  {
      change sent = TRUE
      view(Server) !! req(view = VIEW(Server),x = done)
  }
   handle msg res from majority(Server)   {
      change done = TRUE
  }
   when done handles msg res   {
      drop res
  }
}
role Server {
   handle msg req   {
      req.sender ! res()
  }
}
--------

-------- AFTER rewriteTimer -----------------------
CONST  X,init_servers
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
msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
msg Server -> Client res()


role Client(Server init_servers) {
  state views Set<Server> = init_servers
  state BOOLEAN sent = FALSE
  state BOOLEAN done = FALSE
  when ~(sent) handle event go()  {
      change sent = TRUE
      view(Server) !! req(view = VIEW(Server),x = done)
  }
   handle msg res from majority(Server)   {
      change done = TRUE
  }
   when done handles msg res   {
      drop res
  }
}
role Server {
   handle msg req   {
      req.sender ! res()
  }
}
--------

-------- AFTER RewriteMsgSetHandler ---------------
CONST  X,init_servers
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
msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
msg Server -> Client res()


role Client(Server init_servers) {
  state views Set<Server> = init_servers
  state BOOLEAN sent = FALSE
  state BOOLEAN done = FALSE
  when ~(sent) handle event go()  {
      change sent = TRUE
      view(Server) !! req(view = VIEW(Server),x = done)
  }
   handle msg res from majority(Server)   {
      change done = TRUE
  }
   when done handles msg res   {
      drop res
  }
}
role Server {
   handle msg req   {
      req.sender ! res()
  }
}
--------

-------- AFTER rewriteLifecycle -------------------
CONST  X,init_servers,InitDownClient,CrashClient,StartClient,InitDownServer,CrashServer,StartServer
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
msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
msg Server -> Client res()


role Client(Server init_servers) {
  state views Set<Server> = init_servers
  state BOOLEAN sent = FALSE
  state BOOLEAN done = FALSE
  when ~(sent) handle event go()  {
      change sent = TRUE
      view(Server) !! req(view = VIEW(Server),x = done)
  }
   handle msg res from majority(Server)   {
      change done = TRUE
  }
   when done handles msg res   {
      drop res
  }
}
role Server {
   handle msg req   {
      req.sender ! res()
  }
}
--------

-------- AFTER groupSendInstr ---------------------
CONST  X,init_servers,InitDownClient,CrashClient,StartClient,InitDownServer,CrashServer,StartServer
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
msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
msg Server -> Client res()


role Client(Server init_servers) {
  state views Set<Server> = init_servers
  state BOOLEAN sent = FALSE
  state BOOLEAN done = FALSE
  when ~(sent) handle event go()  {
      change sent = TRUE
      send-group view(Server) !! req(view = VIEW(Server),x = done)
  }
   handle msg res from majority(Server)   {
      change done = TRUE
  }
   when done handles msg res   {
      drop res
  }
}
role Server {
   handle msg req   {
      send-group req.sender ! res()
  }
}
--------

-------- AFTER RewriteExtendHook ------------------
CONST  X,init_servers,InitDownClient,CrashClient,StartClient,InitDownServer,CrashServer,StartServer
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
msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
msg Server -> Client res()


role Client(Server init_servers) {
  state views Set<Server> = init_servers
  state BOOLEAN sent = FALSE
  state BOOLEAN done = FALSE
  when ~(sent) handle event go()  {
      change sent = TRUE
      send-group view(Server) !! req(view = VIEW(Server),x = done)
  }
   handle msg res from majority(Server)   {
      change done = TRUE
  }
   when done handles msg res   {
      drop res
  }
}
role Server {
   handle msg req   {
      send-group req.sender ! res()
  }
}
--------

-------- AFTER Rewrite special operators ----------
CONST  X,init_servers,InitDownClient,CrashClient,StartClient,InitDownServer,CrashServer,StartServer
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
msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
msg Server -> Client res()


role Client(Server init_servers) {
  state views Set<Server> = init_servers
  state BOOLEAN sent = FALSE
  state BOOLEAN done = FALSE
  when ~(sent) handle event go()  {
      change sent = TRUE
      send-group view(Server) !! req(view = VIEW(Server),x = done)
  }
   handle msg res from majority(Server)   {
      change done = TRUE
  }
   when done handles msg res   {
      drop res
  }
}
role Server {
   handle msg req   {
      send-group req.sender ! res()
  }
}
--------

-------- AFTER RewriteOverrideTLA ----------
CONST  X,init_servers,InitDownClient,CrashClient,StartClient,InitDownServer,CrashServer,StartServer
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
msg Client -> Set<Server> req(Set<Server> view,BOOLEAN x)
msg Server -> Client res()


role Client(Server init_servers) {
  state views Set<Server> = init_servers
  state BOOLEAN sent = FALSE
  state BOOLEAN done = FALSE
  when ~(sent) handle event go()  {
      change sent = TRUE
      send-group view(Server) !! req(view = VIEW(Server),x = done)
  }
   handle msg res from majority(Server)   {
      change done = TRUE
  }
   when done handles msg res   {
      drop res
  }
}
role Server {
   handle msg req   {
      send-group req.sender ! res()
  }
}
--------


---- MODULE prim_all1 ----
EXTENDS TLC,Naturals,FiniteSets,Sequences
---- 
CONSTANT X,init_servers,InitDownClient,CrashClient,StartClient,InitDownServer,CrashServer,StartServer,Res_4242(_,_),Req_4242(_,_),Client,Server 
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
---- 
Msg ==
  [type: {"req"},sender: Client,view: SUBSET (Server),x: BOOLEAN] \cup [type: {"res"},sender: Server] 
---- 
ClientState ==
  [sent: BOOLEAN,done: BOOLEAN,g_view_Server: SUBSET (Server),g_inbox: Seq(Msg),g_obuf: Seq(Msg \X (SUBSET (Server)))] 
ServerState ==
  [g_inbox: Seq(Msg)] 
---- 
TypeInvariant ==
  /\ (st_Client \in [Client -> ClientState]) /\ (st_Server \in [Server -> ServerState]) 
Init ==
  /\ (st_Client = [client \in Client |-> [sent |-> FALSE,done |-> FALSE,g_view_Server |-> init_servers,g_inbox |-> <<>>,g_obuf |-> <<>>]]) /\ (st_Server = [server \in Server |-> [g_inbox |-> <<>>]]) 
---- 
Go_4242(client) ==
  /\ (~((st_Client[client]).sent)) /\ (/\ (st_Client' = [st_Client EXCEPT ![client].sent = TRUE,![client].g_obuf = Append(@,<<[type |-> "req",sender |-> client,view |-> (st_Client[client]).g_view_Server,x |-> (st_Client[client]).done],(st_Client[client]).g_view_Server>>)]) /\ (UNCHANGED (<<st_Server>>))) 
ZzZRes_4242(client,msgpos) ==
  /\ (Len((st_Client[client]).g_inbox) > 0)
  /\ (LET is ==
            {local_i \in DOMAIN((st_Client[client]).g_inbox): /\ (((st_Client[client]).g_inbox[local_i]).type = "res") /\ (((st_Client[client]).g_inbox[local_i]).sender \in (st_Client[client]).g_view_Server)} 
          maxi ==
            IF is # {} THEN CHOOSE local_i \in is: \A local_j \in is: local_j <= local_i ELSE 0 
          res ==
            {(st_Client[client]).g_inbox[local_i]: local_i \in is} 
          sender_set ==
            {m.sender: m \in res} 
          KeepMsgTest(m) ==
            m \notin res
       IN /\ (/\ (Majority(sender_set,(st_Client[client]).g_view_Server)) /\ ({} # (st_Client[client]).g_view_Server)) /\ (IF msgpos = 0 THEN /\ (\A local_i \in 1..maxi: TRUE) /\ (/\ (st_Client' = [st_Client EXCEPT ![client].done = TRUE,![client].g_inbox = SelectSeq((st_Client[client]).g_inbox,KeepMsgTest)]) /\ (UNCHANGED (<<st_Server>>))) ELSE maxi # 0 /\ maxi <= msgpos)) 
ZzZRes_4242(client,msgpos) ==
  /\ ((st_Client[client]).done)
  /\ (Len((st_Client[client]).g_inbox) > 0)
  /\ (LET Local_F(local_client,res) ==   /\ (TRUE) /\ (TRUE) /\ (TRUE) local_p ==   IF Head((st_Client[client]).g_inbox).type = "res" THEN 1 ELSE 0  IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET res ==   (st_Client[client]).g_inbox[local_p]  IN /\ (TRUE) /\ (st_Client' = [st_Client EXCEPT ![client].g_inbox = DropPos((st_Client[client]).g_inbox,local_p)]) /\ (UNCHANGED (<<st_Server>>)))) 
ZzZReq_4242(server,msgpos) ==
  /\ (Len((st_Server[server]).g_inbox) > 0)
  /\ (LET Local_F(local_server,req) ==
            /\ (TRUE) /\ (TRUE) 
          local_p ==
            IF Head((st_Server[server]).g_inbox).type = "req" THEN 1 ELSE 0
       IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET req ==   (st_Server[server]).g_inbox[local_p]  IN /\ (st_Client' = [st_Client EXCEPT ![req.sender].g_inbox = Append(@,[type |-> "res",sender |-> server])]) /\ (st_Server' = [st_Server EXCEPT ![server].g_inbox = DropPos((st_Server[server]).g_inbox,local_p)]))) 
---- 
Fairness ==
  /\ (WF_<<st_Client,st_Server>>(\E client \in Client: Go_4242(client))) /\ (WF_<<st_Client,st_Server>>(\E client \in Client: Res_4242(client,0))) /\ (WF_<<st_Client,st_Server>>(\E client \in Client: Res_4242(client,0))) /\ (WF_<<st_Client,st_Server>>(\E server \in Server: Req_4242(server,0))) /\ (WF_<<st_Client,st_Server>>(\E client \in Client: ClientMultiSendToServer(client))) 
---- 
Next ==
  \/ (\E client \in Client: Go_4242(client)) \/ (\E client \in Client: Res_4242(client,0)) \/ (\E client \in Client: Res_4242(client,0)) \/ (\E server \in Server: Req_4242(server,0)) \/ (\E client \in Client: ClientMultiSendToServer(client)) 
---- 
Spec ==
  /\ (Init) /\ ([]([Next]_<<st_Client,st_Server>>)) /\ (Fairness)
====
