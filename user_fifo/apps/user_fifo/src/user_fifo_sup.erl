-module(user_fifo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    {ok, {{rest_for_one, 1, 2}, [?CHILD( user_fifo_server, 
					  user_fifo_server, worker, [48088]),
				  ?CHILD( user_fifo_all_channels_sup, 
					  user_fifo_all_channels_sup, supervisor, [])
				 ]}}.
