#!/bin/sh

mkdir priv

NUM=$RANDOM
EMAIL="user-${NUM}@spotify.com"
openssl req -out priv/cert.pem -new -keyout priv/privkey.pem -newkey rsa:2048 \
    -nodes -x509 \
    -subj "/C=US/ST=NY/L=Brooklyn/O=Spotify/CN=User ${NUM}/emailAddress=${EMAIL}"
echo "$EMAIL" > priv/email
