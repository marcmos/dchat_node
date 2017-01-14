dchat_node – Distributed chat server node
===================================================

## Build dependencies
* rebar3

## Run dependencies
* lfe

## How to compile?
`rebar3 compile`

## How to run?
Application provides standard OTP application behaviour under `dchat_node`.



Run LFE REPL in distributed mode (short-hostname `h1`) with `_build` code path:
```
lfe -sname h1 -pa _build/default/lib/dchat_node/ebin/
```
and run application including dependencies:
```
(application:ensure_all_started 'dchat_node)
```

Mnesia tries to connect to the following nodes by default
* `h1@localhost`
* `h2@localhost`
* `h3@localhost`

## Configuration parameters
* `client_port` – client-to-node TCP listen port, defaults to 0
(system random pick).
