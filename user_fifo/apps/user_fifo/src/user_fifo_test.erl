-module(user_fifo_test).
-export([connect/0, disconnect/1, in/2, out/1]).

connect()->
	{ok, Sock}= gen_tcp:connect("localhost", 48088, [binary, {packet, 0}, {active, false}]),
	Sock.

in(Sock, X ) when is_binary(X) ->
	ok= gen_tcp:send(Sock, <<"in: ", X/bytes>>),
	{ok, Ans}= gen_tcp:recv(Sock, 0),
	Ans;
in(_, _ ) ->
	"The second parameter should be a binary string like <<\"abcd ... \">>".
	
out(Sock) ->
	ok= gen_tcp:send(Sock, <<"out">>),
	{ok, Ans}= gen_tcp:recv(Sock, 0),
	Ans.

disconnect(Sock) -> gen_tcp:close(Sock).
