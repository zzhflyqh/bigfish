-module(mod_rand).
-behaviour(gen_server).
-export([
        start_link/0,
        get_seed/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-record(state, {seed}).

start_link() ->
    gen_server:start_link( {local, ?MODULE}, ?MODULE, [], [] ).

get_seed() ->
    gen_server:call( ?MODULE, get_seed ).

init([]) ->
    State = #state{},
    {ok, State}.

handle_call(get_seed, _From, State) ->
    case State#state.seed of
        undefined -> random:seed(erlang:now());
        S -> random:seed(S)
    end,
    Seed = {random:uniform(99999), random:uniform(999999), random:uniform(999999)},
    {reply, Seed, State#state{seed = Seed}};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.




















