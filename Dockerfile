FROM alpine:latest
RUN apk add erlang openssl
RUN mkdir /app
COPY _build/default/rel/brick_chain/ /app/
COPY genkey.sh /app/
COPY setenv_app.sh /app/
WORKDIR /app
