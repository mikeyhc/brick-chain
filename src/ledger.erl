-module(ledger).

-include("brick.hrl").

-export([install/1, write_brick/1, get_brick/1 ,write_key/2, get_key/1,
         verify_brick/2, create_brick/1]).
-export([stage_brick/1, commit_brick/1]).
-export([self_email/0, self_private_key/0, self_public_key/0]).

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

create_brick(Entries) ->
    Head = head(),
    PrivateKey = self_private_key(),
    case Head of
        undefined -> brick:new(Entries, PrivateKey);
        _ -> brick:new(Entries, Head, PrivateKey)
    end.

stage_brick(Brick) ->
    LedgerPid = brick_chain_sup:get_ledger(),
    ledger_server:stage(LedgerPid, Brick).

commit_brick(Hash) ->
    LedgerPid = brick_chain_sup:get_ledger(),
    ledger_server:commit(LedgerPid, Hash).

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

-spec verify_brick(binary(), brick()) -> ok | {error, Error}
    when Error :: no_key | unknown_previous.
verify_brick(Email, Brick) ->
    Tests = [{{error, unknown_user}, key, fun(_) -> get_key(Email) end},
             {{error, unknown_previous}, fun valid_previous/1},
             {{error, invalid_signature}, fun valid_signature/1},
             {{error, old_head}, fun is_head/1}
            ],
    run_tests(Tests, #{brick => Brick, email => Email}).

self_email() ->
    LedgerPid = brick_chain_sup:get_ledger(),
    ledger_server:email(LedgerPid).

self_private_key() ->
    LedgerPid = brick_chain_sup:get_ledger(),
    ledger_server:private_key(LedgerPid).

self_public_key() ->
    LedgerPid = brick_chain_sup:get_ledger(),
    ledger_server:public_key(LedgerPid).

head() ->
    LedgerPid = brick_chain_sup:get_ledger(),
    ledger_server:head(LedgerPid).

%% helper methods

valid_previous(#{brick := Brick}) ->
    case Brick#brick.previous of
        undefined -> true;
        _ -> get_previous(Brick)
    end.

get_previous(#brick{previous=Previous}) ->
    get_brick(Previous).

valid_signature(#{key := Key, brick := Brick}) ->
    public_key:verify(Brick#brick.hash, sha, Brick#brick.signature, Key).

is_head(#{brick := Brick}) ->
    Brick#brick.previous =:= head().

run_tests([], _State) -> ok;
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
