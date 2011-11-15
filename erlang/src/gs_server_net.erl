-module(gs_server_net).
-export([start/1]).

start([Ip, Port, Sid])->
	ok = start_kernel(), %% kernel, init(such as db, ets...)
	ok = start_disperse([Ip, Port, Sid]), %% line
	ok = start_rand(),
	ok = start_client(),
	ok = start_tcp( Port ),
	ok = start_timer().

start_kernel() ->
	ok.

start_disperse([Ip, Port, Sid]) ->
    io:format(" ~p ~p ~p ~n", [Ip, Port, Sid]),
	ok.

start_rand() ->
	{ok, _} = supervisor:start_child(
						gs_server_sup,
						{mod_rand,
							{mod_rand, start_link, []},
							permanent, 10000, supervisor, [mod_rand]}),
	ok.

start_tcp(Port) ->
	{ok, _} = supervisor:start_child(
					gs_server_sup,
					{gs_server_lister_sup,
						{gs_server_lister_sup, start_link,[]},
						transient, infinity, supervisor, [gs_server_lister_sup]}),
	ok.

start_timer() ->
	{ok, _} = supervisor:start_child(
							gs_server_sup,
							{ timer_frame,
								{timer_frame, start_link, []},
								permanent, 10000, supervisor, [timer_frame]}),
	ok.

start_client() ->
    {ok, _} = supervisor:start_child(
                gs_server_sup,
                {mod_client,
                    {mod_client, start_link, []},
                    permanent, 10000, supervisor, [ mod_client ]}),
    ok.

