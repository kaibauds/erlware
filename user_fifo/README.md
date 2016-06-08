user_fifo
=====

An Erlang/OTP application that provides FIFO queuing service for each client 
that connects to this service listening at a TCP port ( default 48088 ).

Build
-----

$ rebar3 compile

Test
-----

$ rebar3 shell

1> T=user_fifo_test.
user_fifo_test

2> Sock1= T:connect().
#Port<0.18433>

3> Sock2= T:connect().
#Port<0.18487>

4> T:in(Sock1, <<"message 1 for client 1">>).
<<"done">>

5> T:in(Sock2, <<"message 1 for client 2">>).
<<"done">>

6> T:in(Sock2, <<"message 2 for client 2">>).
<<"done">>

7> T:in(Sock2, <<"message 3 for client 2">>).
<<"done">>

8> T:in(Sock1, <<"message 2 for client 1">>).
<<"done">>

9> T:out(Sock1).
<<"message 1 for client 1">>

10> T:out(Sock1).
<<"message 2 for client 1">>

11> T:out(Sock1).
<<"(no more message)">>

12> T:out(Sock2).
<<"message 1 for client 2">>

13> T:out(Sock2).
<<"message 2 for client 2">>

14> T:out(Sock2).
<<"message 3 for client 2">>

15> T:out(Sock2).
<<"(no more message)">>
