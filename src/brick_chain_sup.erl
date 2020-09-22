%%%-------------------------------------------------------------------
%% @doc brick_chain top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(brick_chain_sup).

-behaviour(supervisor).

-export([start_link/3, get_ledger/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(Email, PrivateKey, PublicKey) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE,
                          [Email, PrivateKey, PublicKey]).

get_ledger() ->
    Children = supervisor:which_children(?SERVER),
    {_, Pid, _, _} = lists:keyfind(ledger_server, 1, Children),
    Pid.

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([Email, PrivateKey, PublicKey]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [#{id => ledger_server,
                    start => {ledger_server, start_link,
                              [Email, PrivateKey, PublicKey]}
                   }
                 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
