-module(user_fifo_channel).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {userId, connectionId, coordinatorId, fifoId}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Par={UserId, _ConnectionId, CoordinatorId}) ->
	{ok, ChannelId}= gen_server:start_link(?MODULE, [Par], []),
	gen_server:cast(CoordinatorId, { 'hook channel', {UserId, ChannelId} } ),
	{ok, ChannelId}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([{UserId, ConnectionId, CoordinatorId}]) ->
	process_flag(trap_exit, true),
	{ok, #state{userId=UserId, connectionId= ConnectionId, coordinatorId= CoordinatorId }}.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast({'hook fifo', FifoId}, State) ->
	receiving(),
	{noreply, State#state{fifoId=FifoId}};
handle_cast(receiving, State=#state{userId=UserId, 
				    connectionId=ConnectionId, 
				    coordinatorId= CoordinatorId}) ->
	case gen_tcp:recv(ConnectionId, 0) of
		{ok, Data} -> process(Data),
			      {noreply, State};
		{error, Reason} -> 
			gen_server:cast(CoordinatorId, { 'user logout', UserId }),
			gen_tcp:close(ConnectionId),
			{stop, Reason, State}
	end;
handle_cast({ process, <<"in ", Message/bytes>> }, State=#state{fifoId=FifoId}) ->
	case catch(gen_server:call( FifoId, {in, Message} )) of
		ok -> answer('input done');
		_ -> ok
	end,
	{noreply, State};
handle_cast({ process, <<"out", _/bytes>> }, State=#state{fifoId=FifoId}) ->
	case catch(gen_server:call( FifoId, out )) of
		{ok, empty} ->
			answer(empty);
		{ok, Message} ->
			answer({'output done', Message});
		_ -> ok
	end,
	{noreply, State};
handle_cast({ process, <<"exit", _/bytes>> }, State=#state{userId=UserId, coordinatorId= CoordinatorId}) ->
	gen_server:cast(CoordinatorId, { 'user logout', UserId }),
	{noreply, State};
handle_cast({ process, _ }, State) ->
	answer(illegal),
	{noreply, State};
handle_cast({ answer, AnswerFromFifo }, State=#state{connectionId=ConnectionId}) ->
	AnswerToClient= 
	case AnswerFromFifo of
		'input done' -> <<"done\n">>;
		{ 'output done', Message} -> Message;
		empty -> <<"(no more message)\n">>;
		illegal -> <<"(unknown command)\n">>;
		_ -> <<"(something is wrong)\n">>
	end,
	gen_tcp:send(ConnectionId, AnswerToClient),
	receiving(),
	{noreply, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(Reason, #state{connectionId=ConnectionId} ) ->
	gen_tcp:close(ConnectionId),
	exit(Reason),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
receiving() ->
	gen_server:cast( self(), receiving ).

process(Data) ->
	gen_server:cast( self(), {process, Data} ).

answer(Data) ->
	gen_server:cast( self(), {answer, Data} ).

