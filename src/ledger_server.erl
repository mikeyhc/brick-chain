-module(ledger_server).
-behaviour(gen_server).

-include("brick.hrl").

% public API
-export([start_link/3]).
-export([email/1, private_key/1, public_key/1, head/1]).
-export([stage/2, commit/2]).
% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-record(state, {email :: binary() | string(),
                private_key :: public_key:private_key() | undefined,
                public_key :: public_key:public_key() | undefined,
                staged :: #{binary() => brick()},
                head :: binary() | undefined
               }).

%% public API

start_link(Email, PrivateKey, Certificate) ->
    gen_server:start_link(?MODULE, [Email, PrivateKey, Certificate], []).

email(Pid) ->
    gen_server:call(Pid, email).

private_key(Pid) ->
    gen_server:call(Pid, private_key).

public_key(Pid) ->
    gen_server:call(Pid, public_key).

head(Pid) ->
    gen_server:call(Pid, head).

stage(Pid, Brick) ->
    gen_server:call(Pid, {stage, Brick}).

commit(Pid, Hash) ->
    gen_server:call(Pid, {commit, Hash}).

%% gen_server callbacks

init([Email, PrivateKey, Certificate]) ->
    gen_server:cast(self(), {load_keys, PrivateKey, Certificate}),
    {ok, #state{email=Email, staged=#{}}}.

handle_call(email, _From, State) ->
    {reply, State#state.email, State};
handle_call(private_key, _From, State) ->
    {reply, State#state.private_key, State};
handle_call(public_key, _From, State) ->
    {reply, State#state.public_key, State};
handle_call(head, _From, State) ->
    {reply, State#state.head, State};
handle_call({stage, Brick}, _From, State=#state{staged=Staged}) ->
    NewStaged = Staged#{Brick#brick.hash => Brick},
    {reply, ok, State#state{staged=NewStaged}};
handle_call({commit, Hash}, _From, State=#state{staged=Staged}) ->
    case maps:get(Hash, Staged, false) of
        false -> {reply, {error, not_staged}, State};
        Brick ->
            ledger:write_brick(Brick),
            NewStaged = maps:remove(Hash, Staged),
            {reply, ok, State#state{staged=NewStaged, head=Hash}}
    end.

handle_cast({load_keys, PrivateKeyFile, CertFile}, State) ->
    PrivateKey = brick_crypto:read_pem(PrivateKeyFile),
    Cert = brick_crypto:read_pem(CertFile),
    PublicKey = brick_crypto:cert_to_publickey(Cert),
    {noreply, State#state{private_key=PrivateKey, public_key=PublicKey}}.
