-module(fifo).

-behaviour(gen_server).

%% API
-export([start_link/1, in/2, out/1]).

%% gen_server callbacks
-export([init/1,
	 handle_cast/2,
	 handle_call/3,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

-record(state, {user, qmod, persistor, q}).

%%%===================================================================
%%% API
%%%===================================================================

start_link( {ConnectionId, PersistorId} ) ->
	{ok, FifoId}= gen_server:start_link(?MODULE, [{ConnectionId, simple_queue, PersistorId}], []),
	gen_server:cast(PersistorId, { 'hook fifo', {ConnectionId, FifoId} }),
	{ok, FifoId}.

in(FifoId, Element) ->
	gen_server:cast(FifoId, {in, Element}).

out(FifoId) ->
	gen_server:call(FifoId, out).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([{Qmod}]) ->
	{ok, #state{qmod=Qmod, persistor=underfined, q=Qmod:new()}};
init([{User, Qmod, Persistor}]) ->
	{ok, #state{user=User, qmod=Qmod, persistor=Persistor, 
		    q= case gen_server:call(Persistor, {retrieve, User}) of
			       undefined -> Qmod:new();
			       Q -> Q
		       end
		   }}.

handle_call( out, _From, State=#state{qmod=Qmod, q=Q} ) ->
	case Qmod:out(Q) of
		{undefined, EmptyQ} ->
			{reply, {ok, empty}, State#state{q=EmptyQ} }; 
		{Element, RestQ } ->
			{reply, {ok, Element}, State#state{q=RestQ} } 
	end;
handle_call( {in, Element}, _From,  State=#state{qmod=Qmod, q=Q} ) ->
	{reply, ok, State#state{q=Qmod:in(Element, Q)}};
handle_call( _Msg, _From, State) ->
	{reply, ok, State}.
	
handle_cast( _Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, #state{persistor=underfined}) ->
	ok;
terminate(_Reason, #state{user=User, q=Q, persistor=Persistor}) ->
	gen_server:cast(Persistor, {store, User, Q}),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
