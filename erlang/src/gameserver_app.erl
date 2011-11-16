-module(gameserver_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("common.hrl").
-include("record.hrl").

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(normal,[]) ->
	[Ip, Port, Sid] = init:get_plain_arguments(),
	{ok, SupPid} = gameserver_sup:start_link(),
	gs_server_net:start([Ip, list_to_integer(Port), list_to_integer(Sid)]),
	{ok, SupPid}.

stop( _State ) ->
	void.
