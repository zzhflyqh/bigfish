-module(gs_server_app).
-behaviour(application).
-export([start/2, stop/1]).
-include("common.hrl").
-include("record.hrl").

start(normal,[]) ->
	[Ip, Port, Sid] = init:get_plain_arguments(),
	{ok, SupPid} = gs_server_sup:start_link(),
	gs_server_net:start([Ip, list_to_integer(Port), list_to_integer(Sid)]),
	{ok, SupPid}.

stop( _State ) ->
	void.
