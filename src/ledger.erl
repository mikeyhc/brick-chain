-module(ledger).

-include("brick.hrl").

-export([install/1, write_brick/1, get_brick/1 ,write_key/2, get_key/1,
         verify_brick/2]).

-record(ledger_key, {email :: binary(),
                     key :: public_key:public_key()
                    }).

-spec install([node()]) -> ok.
install(Nodes) ->
    ok = mnesia:create_schema(Nodes),
    rpc:multicall(Nodes, application, start, [mnesia]),
    mnesia:create_table(brick,
                        [{attributes, record_info(fields, brick)},
                         {disc_copies, Nodes}]),
    mnesia:create_table(ledger_key,
                        [{attributes, record_info(fields, ledger_key)},
                         {disc_copies, Nodes}]),
    rpc:multicall(Nodes, application, stop, [mnesia]),
    ok.

-spec write_brick(brick()) -> ok.
write_brick(Brick) ->
    Fn = fun() -> mnesia:write(Brick) end,
    mnesia:activity(transaction, Fn).

-spec get_brick(binary()) -> {ok, brick()} | false.
get_brick(Hash) ->
    Fn = fun() ->
        case mnesia:read({brick, Hash}) of
            [Brick] -> {ok, Brick};
            [] -> false
        end
    end,
    mnesia:activity(transaction, Fn).

-spec write_key(binary(), public_key:public_key()) -> ok.
write_key(Email, Key) ->
    Fn = fun() -> mnesia:write(#ledger_key{email=Email, key=Key}) end,
    mnesia:activity(transaction, Fn).

-spec get_key(binary()) -> {ok, public_key:public_key()} | false.
get_key(Email) ->
    Fn = fun() ->
        case mnesia:read({ledger_key, Email}) of
            [#ledger_key{key=Key}] -> {ok, Key};
            [] -> false
        end
    end,
    mnesia:activity(transaction, Fn).

-spec verify_brick(binary(), brick()) -> true | false | {error, Error}
    when Error :: no_key | unknown_previous.
verify_brick(Email, Brick) ->
    PreviousFn = fun(_) ->
                         case Brick#brick.previous of
                             undefined -> true;
                             _ -> get_previous(Brick)
                         end
                 end,
    Tests = [{{error, no_key}, key, fun(_) -> get_key(Email) end},
             {{error, unknown_previous}, PreviousFn},
             fun(#{key := Key}) -> public_key:verify(Brick#brick.hash, sha,
                                                     Brick#brick.signature,
                                                     Key) end
            ],
    run_tests(Tests, #{}).

%% help methods

get_previous(#brick{previous=Previous}) ->
    get_brick(Previous).

run_tests([], _State) -> true;
run_tests([{Error, Register, Fn}|T], State) ->
    case Fn(State) of
        false -> Error;
        {ok, Value} ->
            run_tests(T, State#{Register => Value})
    end;
run_tests([{Error, Fn}|T], State) ->
    case Fn(State) of
        false -> Error;
        _ -> run_tests(T, State)
    end;
run_tests([Fn|T], State) ->
    case Fn(State) of
        false -> false;
        _ -> run_tests(T, State)
    end.
