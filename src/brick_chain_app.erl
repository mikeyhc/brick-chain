%%%-------------------------------------------------------------------
%% @doc brick_chain public API
%% @end
%%%-------------------------------------------------------------------

-module(brick_chain_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Email = getenv_except("BRICK_EMAIL"),
    PrivateKey = getenv_except("BRICK_PRIVATE_KEY"),
    PublicKey = getenv_except("BRICK_PUBLIC_KEY"),
    brick_chain_sup:start_link(Email, PrivateKey, PublicKey).

stop(_State) ->
    ok.

%% internal functions

getenv_except(Key) ->
    case os:getenv(Key) of
        false -> throw({missing_envvar, Key});
        V -> V
    end.
