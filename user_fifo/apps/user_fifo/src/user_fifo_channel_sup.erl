-module(user_fifo_channel_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
				     transient, 5000, Type, [Mod]}).

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
start_link( Par={_UserId, _ConnectionId, _PersistorId}) ->
    supervisor:start_link(?MODULE, [Par]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([{UserId, ConnectionId, PersistorId}]) ->
	{ok, {{rest_for_one, 2, 2}, [ ?CHILD( {user_fifo_channel, UserId},
					       user_fifo_channel,
					       worker,
					       [{UserId, ConnectionId, PersistorId}]),
				       ?CHILD( {fifo, UserId},
					       fifo,
					       worker,
					       [{UserId, PersistorId}])
				     ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
