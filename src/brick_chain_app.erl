%%%-------------------------------------------------------------------
%% @doc brick_chain public API
%% @end
%%%-------------------------------------------------------------------

-module(brick_chain_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    brick_chain_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
