-module(user_fifo_server).

-behaviour(gen_server).

-define(USER_FIFO_SERVER, ?MODULE).
-define(USER_FIFO_CHANNEL, user_fifo_channel).
-define(USER_FIFO_CHANNEL_SUP, user_fifo_channel_sup).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {listenSock, users} ).
-record(userProp, {channelSup, connectionId, channelId, fifoId, q} ).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Port) ->
	Listen_socket = listen(Port),
	accepting_user(),
	{ok, #state{listenSock=Listen_socket, users= dict:new()}}.

handle_call( {retrieve, UserId}, _, State=#state{users=Users}) ->
	case dict:find( UserId, Users ) of
		error -> 
			{reply, 'not found', State};
		{ok, #userProp{q=Q} } -> 
			{reply, Q, State}
	end;
handle_call( {find_channel, UserId}, _, State=#state{users=Users}) ->
	case dict:find( UserId, Users ) of
		error -> 
			{reply, 'not found', State};
		{ok, #userProp{channelSup=ChannelSup, connectionId=ConnectionId} } -> 
			{reply, {ChannelSup, ConnectionId}, State}
	end;
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(accepting_user, State=#state{listenSock=Listen_socket}) ->
	Coordinator= self(),
	proc_lib:spawn( fun()-> case accept_connection(Listen_socket) of
					{UserId, ConnectionId} ->
						case gen_server:call(Coordinator, {find_channel, UserId}) of
							{OldChannelSup, _} when OldChannelSup=/=undefined -> 
								supervisor:terminate_child(user_fifo_all_channels_sup, OldChannelSup);
							{_, OldConnectionId} when OldConnectionId=/=undefined ->
								gen_tcp:close(OldConnectionId);
							_ -> 
								ok
						end,
						gen_server:cast( Coordinator, {'new connection', {UserId, ConnectionId}} ),
						ChannelSup = user_fifo_all_channels_sup:start_channel_sup(
								    {UserId, ConnectionId, Coordinator}
								   ),
						ok= gen_tcp:controlling_process( ConnectionId, ChannelSup ),
						gen_server:cast( Coordinator, {'update channel supervisor', {UserId, ChannelSup}} );
					_ -> ok
				end
			end ),
	{noreply, State};
handle_cast( {'new connection', {UserId, ConnectionId}}, State=#state{users=Users}) ->
	UserProp=
	case dict:find( UserId, Users ) of
		error -> #userProp{};
		{ok, X} -> X
	end,
	{noreply, State#state{users=dict:store(UserId, 
					       UserProp#userProp{connectionId=ConnectionId,
								 channelId=undefined,
								 fifoId=undefined
								}, Users)} };
handle_cast( {'update channel supervisor', {UserId, ChannelSup}}, State=#state{users=Users}) ->
	{ok,UserProp}=  dict:find( UserId, Users),
	{noreply, State#state{users=dict:store(UserId, UserProp#userProp{channelSup=ChannelSup}, Users)} };
handle_cast( {'user logout', UserId}, State=#state{users=Users}) ->
	{ok, #userProp{channelSup=ChannelSup}}= dict:find( UserId, Users ),
	supervisor:terminate_child(user_fifo_all_channels_sup, ChannelSup),
	{noreply, State};
handle_cast( {'hook fifo', {UserId, FifoId}}, State=#state{users=Users}) ->
	case dict:find( UserId, Users ) of
		error -> {noreply, State};
		{ok, UserProp=#userProp{ channelId=ChannelId } } -> 
			gen_server:cast(ChannelId, {'hook fifo', FifoId}),
			{noreply, State#state{users=dict:store(UserId, UserProp#userProp{fifoId=FifoId}, Users)} }
	end;
handle_cast( {'hook channel', {UserId, ChannelId}}, State=#state{users=Users}) ->
	case dict:find( UserId, Users ) of
		error -> {noreply, State};
		{ok, UserProp=#userProp{ fifoId=FifoId } } -> 
			case FifoId of 
				undefined -> ok;
				_ -> gen_server:cast(ChannelId, {'hook fifo', FifoId})
			end,
			{noreply, State#state{users=dict:store(UserId, UserProp#userProp{channelId=ChannelId}, Users)} }
	end;
handle_cast({store, {UserId, Q}}, State=#state{users=Users}) ->
	case dict:find( UserId, Users ) of
		error -> {noreply, State};
		{ok, UserProp } -> 
			{noreply, State#state{users=dict:store(UserId, UserProp#userProp{q= Q}, Users)} }
	end;
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
accept_connection(LSocket) ->
	{ok, Socket}= gen_tcp:accept(LSocket),
	accepting_user(),
	gen_tcp:send(Socket, <<"Please enter a line in the format \"log in as <user name>\" in a minute or you will be disconnected\n">>),
	case gen_tcp:recv(Socket, 0, 60*1000) of
		{ok, <<"log in as ", X/bytes>>} ->
			[UserName|_]=binary:split(X, <<"\n">>, [trim]),
			gen_tcp:send(Socket, <<"Welcome ", UserName/bytes, "\n">> ),
			{ UserName, Socket };
		_ ->
			gen_tcp:send(Socket, <<"Failed to log in, connection is lcosed, please reconnect, byebye.\n">>),
			error
	end.

accepting_user() ->
	gen_server:cast( ?USER_FIFO_SERVER, accepting_user ).

listen(Port) ->
	{ok, Listen_socket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
	Listen_socket.
