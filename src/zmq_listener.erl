-module(zmq_listener).
-behaviour(gen_server).

%% public API
-export([start_link/3, broadcast/2, broadcast_brick/2]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include("brick.hrl").

-define(TIMEOUT, 10000). % 10 seconds
-define(P2P_PORT, 5557).

-record(peer, {host :: string(),
               socket :: chumak:socket(),
               badcount :: non_neg_integer()
              }).

-record(state, {interface :: string(),
                peers :: [#peer{}] | undefined
               }).

%% public API

start_link(Interface, AnnounceHost, AnnouncePort) ->
    gen_server:start_link(?MODULE, [Interface, AnnounceHost, AnnouncePort], []).

broadcast_brick(Pid, Brick) ->
    Email = ledger:self_email(),
    broadcast(Pid, {brick, Email, Brick}).

broadcast(Pid, Msg) ->
    Ref = make_ref(),
    gen_server:cast(Pid, {broadcast, Ref, self(), Msg}),
    receive
        {broadcast_reply, Ref, Replies} -> {ok, Replies}
    after ?TIMEOUT ->
              {error, timeout}
    end.

%% gen_server callbacks

init([Interface, Host, Port]) ->
    gen_server:cast(self(), {fetch_peers, Host, Port}),
    {ok, #state{interface=Interface}}.

handle_call({peer_msg, Msg}, _From, State) ->
    logger:info("received peer msg: ~p", [Msg]),
    case Msg of
        {brick, Email, Brick} -> {reply, handle_brick(Email, Brick), State}
    end.

handle_cast({fetch_peers, Host, Port}, State) ->
    SelfPid = self(),
    spawn_monitor(fun() -> peer_listener(SelfPid, State#state.interface) end),
    {ok, Socket} = chumak:socket(req, "announce-req"),
    case chumak:connect(Socket, tcp, Host, Port) of
        {ok, _Pid} ->
            Addr = find_addr(State#state.interface),
            Peers = register_peer(Socket, Addr),
            {noreply, State#state{peers=Peers}};
        Other ->
            logger:error("failed to fetch peers: ~p~n", [Other]),
            {stop, Other, State}
    end;
handle_cast({broadcast, Ref, Pid, Msg}, State) ->
    Fn = fun(Peer) -> broadcast_(Peer, Ref, Msg) end,
    lists:foreach(Fn, State#state.peers),
    NewPeers = receive_peers(Ref, Pid, State#state.peers),
    {noreply, State#state{peers=NewPeers}};
handle_cast({forward, Brick}, State) ->
    {ok, Results} = broadcast_brick(self(), Brick),
    Oks = lists:filter(fun(X) -> X =:= ok end, Results),
    Quorem = trunc(length(State#state.peers)),
    if Oks < Quorem -> ok; % TODO rollback
       true -> ledger:commit_brick(Brick#brick.hash)
    end,
    {noreply, State}.

handle_info({broadcast_reply, Ref, _Replies}, State) ->
    logger:warn("late reply to ~p", [Ref]),
    {noreply, State}.

register_peer(Socket, Addr) ->
    EncodedAddr = inet:ntoa(Addr),
    ok = chumak:send(Socket, erlang:term_to_binary({register, EncodedAddr})),
    {ok, Data} = chumak:recv(Socket),
    Builder = fun(Host) ->
        {ok, PeerSocket} = chumak:socket(req, Host ++ "-req"),
        {ok, _Pid} = chumak:connect(PeerSocket, tcp, Host, ?P2P_PORT),
        #peer{host=Host, socket=PeerSocket, badcount=0}
    end,
    {peerlist, Peers} = erlang:binary_to_term(Data, [safe]),
    lists:map(Builder, Peers).

%% helper methods

broadcast_(#peer{host=Host, socket=Socket}, Ref, Msg) ->
    SelfPid = self(),
    Fn = fun() ->
        ok = chumak:send(Socket, erlang:term_to_binary(Msg)),
        {ok, Data} = chumak:recv(Socket),
        SelfPid ! {peer_reply, Host, Ref, Data}
    end,
    spawn(Fn).

receive_peers(Ref, Pid, Peers) ->
    Hosts = lists:map(fun(P) -> P#peer.host end, Peers),
    BadHosts = receive_peers(Ref, Pid, Hosts, []),
    lists:foldl(fun mark_badhosts/2, Peers, BadHosts).

receive_peers(Ref, Pid, [], Replies) ->
    Pid ! {broadcast_reply, Ref, Replies},
    [];
receive_peers(Ref, Pid, Hosts, Replies) ->
    receive
        {peer_reply, Host, Ref, Reply} ->
            NewHosts = lists:delete(Host, Hosts),
            receive_peers(Ref, Pid, NewHosts, [Reply|Replies])
    after ?TIMEOUT ->
        Hosts
    end.

mark_badhosts(Host, Peers) ->
    logger:info("marking badhost ~s", [Host]),
    Fn = fun(P=#peer{host=H}) when H =:= Host ->
                 P#peer{badcount=P#peer.badcount+1};
            (P) -> P
         end,
    lists:map(Fn, Peers).

find_addr(Interface) ->
    {ok, IfAddrs} = inet:getifaddrs(),
    {_, Opts} = lists:keyfind(Interface, 1, IfAddrs),
    {_, Addr} = lists:keyfind(addr, 1, Opts),
    Addr.

peer_listener(Pid, Interface) ->
    Addr = inet:ntoa(find_addr(Interface)),
    {ok, Socket} = chumak:socket(rep, "broadcast-rep"),
    {ok, _Pid} = chumak:bind(Socket, tcp, Addr, ?P2P_PORT),
    listener_loop(Socket, Pid).

listener_loop(Socket, Pid) ->
    {ok, Data} = chumak:recv(Socket),
    Msg = erlang:binary_to_term(Data, [safe]),
    Reply = gen_server:call(Pid, {peer_msg, Msg}),
    ok = chumak:send(Socket, erlang:term_to_binary(Reply)),
    listener_loop(Socket, Pid).

handle_brick(Email, Brick) ->
    case ledger:get_brick(Brick#brick.hash) of
        {ok, _Brick} -> ok;
        false ->
            case ledger:verify_brick(Email, Brick) of
                ok ->
                    ledger:stage_brick(Brick),
                    gen_server:cast(self(), {forward, Brick}),
                    ok;
                Err ->
                    Err
                    % TODO reject brick with err
            end
    end.
