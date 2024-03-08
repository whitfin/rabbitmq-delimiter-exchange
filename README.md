# RabbitMQ Delimiter Exchange Type

_Update 2024: it looks like as of RabbitMQ v3.13 there is no long any
meaningful difference between the use of `x-delimiter` exchanges and
the `CC` header inside a `direct` exchange. The assumption is that this
is a result of the [performance optimizations](https://www.rabbitmq.com/blog/2024/01/11/3.13-release#caveat)
made in the v3.13 release. For this reason if you are targeting RabbitMQ
>= v3.13, I would suggest using `direct` exchanges with `CC` instead._

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

In v3.11.x, RabbitMQ introduced performance improvements for the base direct
exchange type. This is currently incompatible with this plugin, due to some
internal namespacing (it's only enabled on "direct" exchanges). I have filed
for some customization here, but in the meantime this plugin will use the
implementation of direct exchanges included in <= 3.10.x. Hopefully this can
be changed in future to also see the same improvements :).

## Installation

The [RabbitMQ documentation](https://www.rabbitmq.com/installing-plugins.html)
explains how to install plugins into your server application. Every plugin is
packaged as either a `.ez` file or a plain directory, based on your version
of RabbitMQ. Just build it and drop the output plugin into your server plugins
directory.

You don't need to clone the RabbitMQ umbrella project; just clone
this repository, check out the branch you want (i.e. `v3.8.x`), and run `make`.
If there is no existing branch for your version, just create it from `main`;
RabbitMQ checks this version when pulling and pinning libraries. This plugin
targets RabbitMQ 3.6.0 and later versions.

## Development

This repository includes some Docker setup to make it easier to test the plugin,
and run a server with the plugin installed. Packaging the plugin is pretty simple
using Docker:

```bash
# build a development image with dependencies
$ cat Dockerfile.build | \
    docker build -t rabbitmq-build -f - .

# attach to the container
$ docker run -it --rm \
    -v $PWD:/opt/rabbitmq \
    -w /opt/rabbitmq \
    rabbitmq-build

# build and package
$ make
$ make dist
```

If you want to start a RabbitMQ server with this plugin enabled, you can use
the server Dockerfile to let you run the tests against it:

```bash
# build a development image with dependencies
$ cat Dockerfile.build Dockerfile.service | \
    docker build -t rabbitmq-server -f - .

# start running to the container
$ docker run -it --rm \
    -p 15672:15672 \
    -p 5672:5672 \
    rabbitmq-server

# test the plugin
$ npm test
```

There are other ways to embed your workflow into the main server tree, but this
seemed complicated for how simple this plugin is, so the above worked for me.
