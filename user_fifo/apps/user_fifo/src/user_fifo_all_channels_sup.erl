-module(user_fifo_all_channels_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_channel_sup/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
				     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_channel_sup(Par={_UserId, _ConnectionId, _PersistorId}) ->
	case supervisor:start_child(?MODULE, [Par]) of
		{ok, X} -> X;
		{ok, Y, _} -> Y
	end.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
	{ok, {{simple_one_for_one, 1, 2}, 
	      [
	       ?CHILD( ignored, 
		       user_fifo_channel_sup,
		       supervisor,
		       []) 
	      ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
