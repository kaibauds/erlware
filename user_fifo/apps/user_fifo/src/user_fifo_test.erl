-module(user_fifo_test).
-export([login/1, logout/1, in/2, out/1]).

login(User)->
	{ok, Sock}= gen_tcp:connect("localhost", 48088, [binary, {packet, 0}, {active, false}]),
	gen_tcp:recv(Sock, 0),
	UserBin= list_to_binary(User),
	gen_tcp:send(Sock, <<"log in as ", UserBin/bytes>>),
	gen_tcp:recv(Sock, 0),
	Sock.

in(Sock, X ) when is_binary(X) ->
	ok= gen_tcp:send(Sock, <<"in ", X/bytes>>),
	{ok, Ans}= gen_tcp:recv(Sock, 0),
	Ans;
in(_, _ ) ->
	"The second parameter should be a binary string like <<\"abcd ... \">>".
	
out(Sock) ->
	ok= gen_tcp:send(Sock, <<"out">>),
	{ok, Ans}= gen_tcp:recv(Sock, 0),
	Ans.

logout(Sock) -> gen_tcp:close(Sock).
