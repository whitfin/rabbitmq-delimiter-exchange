# RabbitMQ Delimiter Exchange Type

## What It Does

This plugin adds an exchange type allowing for multiple routing keys
on a message via delimiter based separation.

This covers the situation in which you want messages to be routable
via multiple fields, without having to resort to using the headers
exchange (which has bad performance, comparitively). You can listen
on any routing key in the delimited list, and only receive a single
copy of the message into your queue/exchange even if multiple bindings
match.

It's backed by the same logic used by the direct exchange, just with
support for delimiters. Performance tests with the official RabbitMQ
[benchmarking tool](https://github.com/rabbitmq/rabbitmq-perf-test)
show almost exactly the same throughput as direct exchanges (with
the same number of total matching bindings).

## How It Works

Due to RabbitMQ messages only allowing for a single routing key, this
plugin just splits on a delimiter to enable multiple routing keys in
the single routing key field. From that point it just routes in the
same way that a direct exchange would, thus resulting in roughly the
same throughput and performance.

When you bind to an exchange you simply specify your routing key to
listen on as usual, even if it's a sub-key in the routing key split.
When you publish a message you should start your key with a colon,
(`:`) and then also separate any routing keys with a colon.

You can control the delimiter being used by changing the first character
inside the routing key; it's recommended to use the colon, but in case
of clashes you can pick your own. In the case below, the message would
be routed to any bindings for both `one` and `two`:

```erlang
% gets routed to "one" and "two" bindings
#basic_message{routing_keys = [<<":one:two">>]}

% same routing with a "," as delimiter
#basic_message{routing_keys = [<<",one,two">>]}
```

The delimiter is included in the routing key itself in order to make the
key self-describing, meaning that we don't have to a) look for the delimiter
in the bindings, b) look for the delimiter in the headers, and c) we can
allow different delimiters on a per-message basis (for example if you have
arbitrary routing keys inside your system).

To use the exchange the type is "x-delimiter" and there are currently no
arguments being used, so any provided are ignored. It should work with many
versions of RabbitMQ as it's backed by the same implementation as the direct
exchange. If you see any incompatibility, please file an issue.

## Installation

The [RabbitMQ documentation](https://www.rabbitmq.com/installing-plugins.html)
explains how to install plugins into your server application. Each plugin is
packaged as a .ez file; you can download the latest "final" archive from the
[releases](https://github.com/whitfin/rabbitmq-delimiter-exchange/releases)
page on GitHub, or build it yourself directly from source. To do so you don't
need to clone the RabbitMQ umbrella; just clone this repository and run the
`make dist` build task. Building outside of the repository requires that you
define the `rabbit_common` version via the `$RABBITMQ_REF` variable.

## Development

To work on this plugin (and plugins in general), you should use the RabbitMQ
[umbrella project](https://github.com/rabbitmq/rabbitmq-public-umbrella). To
set up the plugin for development, you can follow these steps:

```bash
# setup the umbrella project for the RabbitMQ ecosystem
$ git clone https://github.com/rabbitmq/rabbitmq-public-umbrella
$ cd rabbitmq-public-umbrella
$ make co
$ make

# clone the plugin for development
$ cd deps
$ git clone https://github.com/whitfin/rabbitmq-delimiter-exchange
$ mv rabbitmq-delimiter-exchange rabbitmq_delimiter_exchange
$ cd rabbitmq_delimiter_exchange

# build the plugin
$ make

# package the plugin
$ make dist

# run the server with the plugin enabled
$ cd ../rabbitmq_server_release
$ make run-broker PLUGINS='rabbitmq_delimiter_exchange'
```
