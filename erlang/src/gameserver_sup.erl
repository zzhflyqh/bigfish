
-module(gameserver_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

start_child( Mod ) ->
	start_child(Mod, []).

start_child( Mod, Args ) ->
	{ok, _} = supervisor:start_child(?MODULE,
							{Mod, {Mod,start_link, Args},
							transient, 100, worker, [Mod]}),
	ok.


init([]) ->
    {ok, { {one_for_one, 3, 10}, []} }.

