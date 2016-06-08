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

-record(state, {listen_socket, clients} ).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Port) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Port, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Port) ->
%%	timer:sleep(1000),
	Listen_socket = listen(Port),
	accepting_client(),
	{ok, #state{listen_socket=Listen_socket, clients= dict:new()}}.

handle_call( {retrieve, ConnectionId},  FifoId, State=#state{clients=Clients}) ->
	case dict:find( ConnectionId, Clients ) of
		error -> 
			exit(FifoId, shutdown),
			{reply, ok, State};
		{ok, { _, _, Q} } -> 
			{reply, Q, State}
	end;
handle_call(_Request, _From, State) ->
	Reply = ok,
	{reply, Reply, State}.

handle_cast(accepting_client, State=#state{listen_socket=Listen_socket}) ->
	PersistorId= self(),
	proc_lib:spawn( fun()-> ConnectionId = accept_connection(Listen_socket),
				ok= gen_tcp:controlling_process( ConnectionId, PersistorId ),
				gen_server:cast( PersistorId, {'new client', ConnectionId} ),
				user_fifo_all_channels_sup:start_channel_sup({ConnectionId, PersistorId}),
				accepting_client()
			end ),
	{noreply, State};
handle_cast( {'new client', ConnectionId}, State=#state{clients=Clients}) ->
	{noreply, State#state{clients=dict:store(ConnectionId, {undefined, undefined, undefined}, Clients)} };
handle_cast( {'hook fifo', {ConnectionId, FifoId}}, State=#state{clients=Clients}) ->
	case dict:find( ConnectionId, Clients ) of
		error -> {noreply, State};
		{ok, { ChannelId, _, Q} } -> 
			gen_server:cast(ChannelId, {'hook fifo', FifoId}),
			{noreply, State#state{clients=dict:store(ConnectionId, {ChannelId, FifoId, Q}, Clients)} }
	end;
handle_cast( {'hook channel', {ConnectionId, ChannelId}}, State=#state{clients=Clients}) ->
	case dict:find( ConnectionId, Clients ) of
		error -> {noreply, State};
		{ok, { _, FifoId, Q} } -> 
			case FifoId of 
				undefined -> ok;
				_ -> gen_server:cast(ChannelId, {'hook fifo', FifoId})
			end,
			{noreply, State#state{clients=dict:store(ConnectionId, {ChannelId, FifoId, Q}, Clients)} }
	end;
handle_cast( {store, {ConnectionId, Q}}, State=#state{clients=Clients}) ->
	case dict:find( ConnectionId, Clients ) of
		error -> {noreply, State};
		{ok, { ChannelId, FifoId, _} } -> 
			{noreply, State#state{clients=dict:store(ConnectionId, {ChannelId, FifoId, Q}, Clients)} }
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
	Socket.

accepting_client() ->
	gen_server:cast( ?USER_FIFO_SERVER, accepting_client ).

listen(Port) ->
	{ok, Listen_socket} = gen_tcp:listen(Port, [binary, {packet, 0}, {active, false}]),
	Listen_socket.
