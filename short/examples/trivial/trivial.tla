---- PARTS ----
protocol trivial concern default {
  roles Client,Server
  constant NIL,init_Server
  interaction core {
    msg Client -> Server rd()
    msg Server -> Client rdRes()
    display swimlane {
      msg rd color = blue
      msg rdRes color = green
    }
    role Client {
      state views Server = init_Server
      handle event clientSeq(BOOLEAN v)  {
          view(Server) ! rd()
           await msg rdRes from all(Server)   
      }
    }
    role Server {
      state BOOLEAN cvalue = FALSE
       handle msg rd   {
          reply rdRes(val = cvalue)
      }
    }
  }
}
--------


-------- AFTER Merging interactions ---------------
protocol trivial concern __generated {
  constant NIL,init_Server
  roles Client,Server
  interaction __generated {
    msg Client -> Server rd()
    msg Server -> Client rdRes()
    display swimlane {
      msg rd color = blue
      msg rdRes color = green
    }
    role Client {
      state views Server = init_Server
      handle event clientSeq(BOOLEAN v)  {
          view(Server) ! rd()
           await msg rdRes from all(Server)   
      }
    }
    role Server {
      state BOOLEAN cvalue = FALSE
       handle msg rd   {
          reply rdRes(val = cvalue)
      }
    }
  }
}
--------

-------- AFTER insideOut (flatten) ----------------
CONST  NIL,init_Server

msg Client -> Server rd()
msg Server -> Client rdRes()

display swimlane {
  msg rd color = blue
  msg rdRes color = green
}
role Client {
  state views Server = init_Server
  handle event clientSeq(BOOLEAN v)  {
      view(Server) ! rd()
       await msg rdRes from all(Server)   
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
   handle msg rd   {
      reply rdRes(val = cvalue)
  }
}
--------

-------- AFTER rewriteWhen ------------------------
CONST  NIL,init_Server

msg Client -> Server rd()
msg Server -> Client rdRes()

display swimlane {
  msg rd color = blue
  msg rdRes color = green
}
role Client {
  state views Server = init_Server
  handle event clientSeq(BOOLEAN v)  {
      view(Server) ! rd()
       await msg rdRes from all(Server)   
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
   handle msg rd   {
      reply rdRes(val = cvalue)
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
msg Client -> Server rd()
msg Server -> Client rdRes()

display swimlane {
  msg rd color = blue
  msg rdRes color = green
}
role Client {
  state views Server = init_Server
  handle event clientSeq(BOOLEAN v)  {
      view(Server) ! rd()
       await msg rdRes from all(Server)   
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
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
msg Client -> Server rd()
msg Server -> Client rdRes()

display swimlane {
  msg rd color = blue
  msg rdRes color = green
}
role Client {
  state views Server = init_Server
  handle event clientSeq(BOOLEAN v)  {
      view(Server) ! rd()
       await msg rdRes from all(Server)   
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
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
msg Client -> Server rd()
msg Server -> Client rdRes()

display swimlane {
  msg rd color = blue
  msg rdRes color = green
}
role Client {
  state views Server = init_Server
  handle event clientSeq(BOOLEAN v)  {
      view(Server) ! rd()
       await msg rdRes from all(Server)   
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
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
msg Client -> Server rd()
msg Server -> Client rdRes()

display swimlane {
  msg rd color = blue
  msg rdRes color = green
}
role Client {
  state views Server = init_Server
  when g_pc_clientSeq = 0 handle event clientSeq(BOOLEAN v)  {
      view(Server) ! rd()
      change g_pc_clientSeq = 1
  }
  state (0..1) g_pc_clientSeq = 0
   when g_pc_clientSeq = 1 handles msg rdRes from all(Server)   {
      change g_pc_clientSeq = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
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
msg Client -> Server rd()
msg Server -> Client rdRes()

display swimlane {
  msg rd color = blue
  msg rdRes color = green
}
role Client {
  state views Server = init_Server
  when g_pc_clientSeq = 0 handle event clientSeq(BOOLEAN v)  {
      view(Server) ! rd()
      change g_pc_clientSeq = 1
  }
  state (0..1) g_pc_clientSeq = 0
   when g_pc_clientSeq = 1 handles msg rdRes from all(Server)   {
      change g_pc_clientSeq = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
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
msg Client -> Server rd()
msg Server -> Client rdRes()

display swimlane {
  msg rd color = blue
  msg rdRes color = green
}
role Client {
  state views Server = init_Server
  when g_pc_clientSeq = 0 handle event clientSeq(BOOLEAN v)  {
      view(Server) ! rd()
      change g_pc_clientSeq = 1
  }
  state (0..1) g_pc_clientSeq = 0
   when g_pc_clientSeq = 1 handles msg rdRes from all(Server)   {
      change g_pc_clientSeq = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
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
msg Client -> Server rd()
msg Server -> Client rdRes()

display swimlane {
  msg rd color = blue
  msg rdRes color = green
}
role Client {
  state views Server = init_Server
  when g_pc_clientSeq = 0 handle event clientSeq(BOOLEAN v)  {
      view(Server) ! rd()
      change g_pc_clientSeq = 1
  }
  state (0..1) g_pc_clientSeq = 0
   when g_pc_clientSeq = 1 handles msg rdRes from all(Server)   {
      change g_pc_clientSeq = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
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
msg Client -> Server rd()
msg Server -> Client rdRes()

display swimlane {
  msg rd color = blue
  msg rdRes color = green
}
role Client {
  state views Server = init_Server
  when g_pc_clientSeq = 0 handle event clientSeq(BOOLEAN v)  {
      view(Server) ! rd()
      change g_pc_clientSeq = 1
  }
  state (0..1) g_pc_clientSeq = 0
   when g_pc_clientSeq = 1 handles msg rdRes from all(Server)   {
      change g_pc_clientSeq = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
   handle msg rd   {
      rd.sender ! rdRes(val = cvalue)
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
msg Client -> Server rd()
msg Server -> Client rdRes()

display swimlane {
  msg rd color = blue
  msg rdRes color = green
}
role Client {
  state views Server = init_Server
  when g_pc_clientSeq = 0 handle event clientSeq(BOOLEAN v)  {
      change g_pc_clientSeq = 1
      send-group view(Server) ! rd()
  }
  state (0..1) g_pc_clientSeq = 0
   when g_pc_clientSeq = 1 handles msg rdRes from all(Server)   {
      change g_pc_clientSeq = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
   handle msg rd   {
      send-group rd.sender ! rdRes(val = cvalue)
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
msg Client -> Server rd()
msg Server -> Client rdRes()

display swimlane {
  msg rd color = blue
  msg rdRes color = green
}
role Client {
  state views Server = init_Server
  when g_pc_clientSeq = 0 handle event clientSeq(BOOLEAN v)  {
      change g_pc_clientSeq = 1
      send-group view(Server) ! rd()
  }
  state (0..1) g_pc_clientSeq = 0
   when g_pc_clientSeq = 1 handles msg rdRes from all(Server)   {
      change g_pc_clientSeq = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
   handle msg rd   {
      send-group rd.sender ! rdRes(val = cvalue)
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
msg Client -> Server rd()
msg Server -> Client rdRes()

display swimlane {
  msg rd color = blue
  msg rdRes color = green
}
role Client {
  state views Server = init_Server
  when g_pc_clientSeq = 0 handle event clientSeq(BOOLEAN v)  {
      change g_pc_clientSeq = 1
      send-group view(Server) ! rd()
  }
  state (0..1) g_pc_clientSeq = 0
   when g_pc_clientSeq = 1 handles msg rdRes from all(Server)   {
      change g_pc_clientSeq = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
   handle msg rd   {
      send-group rd.sender ! rdRes(val = cvalue)
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
msg Client -> Server rd()
msg Server -> Client rdRes()

display swimlane {
  msg rd color = blue
  msg rdRes color = green
}
role Client {
  state views Server = init_Server
  when g_pc_clientSeq = 0 handle event clientSeq(BOOLEAN v)  {
      change g_pc_clientSeq = 1
      send-group view(Server) ! rd()
  }
  state (0..1) g_pc_clientSeq = 0
   when g_pc_clientSeq = 1 handles msg rdRes from all(Server)   {
      change g_pc_clientSeq = 0
  }
}
role Server {
  state BOOLEAN cvalue = FALSE
   handle msg rd   {
      send-group rd.sender ! rdRes(val = cvalue)
  }
}
--------


---- MODULE trivial ----
EXTENDS TLC,Naturals,FiniteSets,Sequences
---- 
CONSTANT NIL,init_Server,InitDownClient,CrashClient,StartClient,InitDownServer,CrashServer,StartServer,RdRes_4242(_,_),Rd_4242(_,_),Client,Server 
VARIABLE st_Client,st_Server 
---- 
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
  [type: {"rd"},sender: Client] \cup [type: {"rdRes"},sender: Server] 
---- 
ClientState ==
  [g_pc_clientSeq: (0..1),g_view_Server: Server,g_inbox: Seq(Msg)] 
ServerState ==
  [cvalue: BOOLEAN,g_inbox: Seq(Msg)] 
---- 
TypeInvariant == TRUE
TypeInvariant2 ==
  /\ (st_Client \in [Client -> ClientState]) /\ (st_Server \in [Server -> ServerState]) 
Init ==
  /\ (st_Client = [client \in Client |-> [g_pc_clientSeq |-> 0,g_view_Server |-> init_Server,g_inbox |-> <<>>]]) /\ (st_Server = [server \in Server |-> [cvalue |-> FALSE,g_inbox |-> <<>>]]) 
---- 
ClientSeq_4242(client,v) ==
  /\ ((st_Client[client]).g_pc_clientSeq = 0) /\ (/\ (st_Client' = [st_Client EXCEPT ![client].g_pc_clientSeq = 1]) /\ (st_Server' = [st_Server EXCEPT ![(st_Client[client]).g_view_Server].g_inbox = Append(@,[type |-> "rd",sender |-> client])])) 
ZzZRdRes_4242(client,msgpos) ==
  /\ ((st_Client[client]).g_pc_clientSeq = 1)
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
       IN /\ (/\ (sender_set = (st_Client[client]).g_view_Server) /\ ({} # (st_Client[client]).g_view_Server)) /\ (IF msgpos = 0 THEN /\ (\A local_i \in 1..maxi: TRUE) /\ (/\ (st_Client' = [st_Client EXCEPT ![client].g_pc_clientSeq = 0,![client].g_inbox = SelectSeq((st_Client[client]).g_inbox,KeepMsgTest)]) /\ (UNCHANGED (<<st_Server>>))) ELSE maxi # 0 /\ maxi <= msgpos)) 
ZzZRd_4242(server,msgpos) ==
  /\ (Len((st_Server[server]).g_inbox) > 0)
  /\ (LET Local_F(local_server,rd) ==
            /\ (TRUE) /\ (TRUE) 
          local_p ==
            IF Head((st_Server[server]).g_inbox).type = "rd" THEN 1 ELSE 0
       IN /\ (local_p > 0) /\ (IF msgpos = 0 THEN TRUE ELSE local_p = msgpos) /\ (LET rd ==   (st_Server[server]).g_inbox[local_p]  IN /\ (st_Client' = [st_Client EXCEPT ![rd.sender].g_inbox = Append(@,[type |-> "rdRes",sender |-> server,val |-> (st_Server[server]).cvalue])]) /\ (st_Server' = [st_Server EXCEPT ![server].g_inbox = DropPos((st_Server[server]).g_inbox,local_p)]))) 
---- 
Fairness ==
  /\ (WF_<<st_Client,st_Server>>(\E client \in Client,v \in BOOLEAN: ClientSeq_4242(client,v))) /\ (WF_<<st_Client,st_Server>>(\E client \in Client: RdRes_4242(client,0))) /\ (WF_<<st_Client,st_Server>>(\E server \in Server: Rd_4242(server,0))) 
---- 
Next ==
  \/ (\E client \in Client,v \in BOOLEAN: ClientSeq_4242(client,v)) \/ (\E client \in Client: RdRes_4242(client,0)) \/ (\E server \in Server: Rd_4242(server,0)) 
---- 
Spec ==
  /\ (Init) /\ ([]([Next]_<<st_Client,st_Server>>)) /\ (Fairness)
====
