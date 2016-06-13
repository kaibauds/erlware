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

-record(state, {connectionId, persistorId, fifoId}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Par={ConnectionId, PersistorId}) ->
	{ok, ChannelId}= gen_server:start_link(?MODULE, [Par], []),
	gen_server:cast(PersistorId, { 'hook channel', {ConnectionId, ChannelId} } ),
	{ok, ChannelId}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([{ConnectionId, PersistorId}]) ->
	{ok, #state{connectionId= ConnectionId, persistorId= PersistorId }}.

handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast({'hook fifo', FifoId}, State) ->
	receiving(),
	{noreply, State#state{fifoId=FifoId}};
handle_cast(receiving, State=#state{connectionId=ConnectionId}) ->
	case gen_tcp:recv(ConnectionId, 0) of
		{ok, Data} -> process(Data),
			      {noreply, State};
		{error, Reason} -> 
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
handle_cast({ process, <<"exit", _/bytes>> }, State=#state{connectionId=ConnectionId, persistorId= PersistorId}) ->
	gen_server:cast(PersistorId, { 'drop connectionId', ConnectionId }),
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
	io:format("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-~nhey, received something: ~w~n", [_Info]),
	{noreply, State}.

terminate(_Reason, _State) ->
	io:format("---------------------------------~nhey, termination is running for the reason: ~w~n", [_Reason]),
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

