-module(gs_server_listner_sup).
-behaviour(supervisor).
-export([start_link/1]).
-export([init/1]).

start_link(Port) ->
	supervisor:start_link(?MODULE, {10, Port}).

init({AcCount, Port}) ->
	{ok,
		{{one_for_all, 10, 10},
			[
				{
					gs_server_accept_sup,
					{gs_server_accept_sup, start_link,[]},
					transient,
					supervisor,
					[gs_server_accept_sup]
				},
				{
					gs_server_listner,
					{gs_server_listner, start_link, [AcCount, Port]},
					transient,
					100,
					worker,
					[gs_server_listner]
				}
			]
		}
	}.
