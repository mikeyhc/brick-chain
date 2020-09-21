-module(brick).

-include("brick.hrl").

-export([new/2, new/3, to_map/1]).

-spec new([iodata()], public_key:private_key()) -> brick().
new(Entries, PrivateKey) ->
    Hash = lists:foldl(fun hash_append/2, <<>>, Entries),
    Sig = public_key:sign(Hash, sha, PrivateKey),
    #brick{entries=Entries, hash=Hash, signature=Sig}.

-spec new([iodata()], binary(), public_key:private_key()) -> brick().
new(Entries, PreviousHash, PrivateKey) ->
    Hash = lists:foldl(fun hash_append/2, <<>>, [PreviousHash|Entries]),
    Sig = public_key:sign(Hash, sha, PrivateKey),
    #brick{entries=Entries, previous=PreviousHash, hash=Hash, signature=Sig}.

-spec to_map(brick()) -> #{atom() => iodata()}.
to_map(#brick{entries=Entries, previous=Previous, hash=Hash, signature=Sig}) ->
    #{entries => Entries,
      previous => Previous,
      hash => base64:encode(Hash),
      signature => base64:encode(Sig)
     }.

%% helper methods

-spec hash_append(iodata(), binary()) -> binary().
hash_append(New, Acc) ->
    crypto:hash(sha512, [New, Acc]).
