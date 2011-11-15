-module(gs_server_accept_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{simple_one_for_one, 10, 10},
			[{gs_server_accpt,{gs_server_accept, start_link, []},
					transient, brutal_kill, worker, [gs_server_accept]}]}}.
