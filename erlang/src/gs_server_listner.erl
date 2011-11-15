-module(gs_server_listner).
-behaviour(gen_server).
-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handl_info/2,
		terminate/2, code_change/3]).
-include("common.hrl").

start_link( AcCount, Port ) ->
	gen_server:start_link(?MODULE, {AcCount, Port}, []).

init({AcCount, Port}) ->
	process_flag(trap_exit, true),
	case gen_tcp:listen(Port, ?TCP_OPTIONS) of
		{ok, LSock} ->
			lists:foreach(fun (_) ->
						{ok, _Pid} = supervisor:start_child(
							gs_server_accept_sup, [LSock])
				end,
				lists:duplicate(AcCount, dummy)),
			{ok, LSock};
		{error, Reason} ->
			{stop, {cannot_listen, Reason}}
	end.

handle_call( _Req, _From, State) ->
	{reply, State, State}.

handle_cast( _Msg, State ) ->
	{noreply, State}.

terminate(_Reason, State) ->
	gen_tcp:close(State),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
