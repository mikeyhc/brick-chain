-module(brick_crypto).

-export([read_pem/1, cert_to_publickey/1]).

-include_lib("public_key/include/public_key.hrl").

read_pem(Filename) ->
    {ok, PemBin} = file:read_file(Filename),
    [Entry] = public_key:pem_decode(PemBin),
    public_key:pem_entry_decode(Entry).

cert_to_publickey(Cert) ->
    TBS = Cert#'Certificate'.tbsCertificate,
    KeyInfo = TBS#'TBSCertificate'.subjectPublicKeyInfo,
    DerKey = KeyInfo#'SubjectPublicKeyInfo'.subjectPublicKey,
    public_key:der_decode('RSAPublicKey', DerKey).
