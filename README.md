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

In LFE REPL type:
`(application:start 'dchat_node)`

## Configuration parameters
* `node_port` – node-to-node TCP listen port
* `client_port` – client-to-node TCP listen port
