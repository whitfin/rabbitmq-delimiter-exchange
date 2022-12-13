%% This exchange implementation is essentially just a direct exchange
%% allowing for multiple routing keys per message. There appears to be
%% some form of support for multiple routing keys within rabbitmq-server,
%% but appears unsupported by clients in general.
%%
%% In order to maximise performance, your routing keys must be self-describing
%% by providing the delimiter you wish to use as the first character. This aids
%% performance by avoiding a lookup in the bindings table, or in the message
%% headers, but with the limitation that (currently) you can only have single
%% character delimiters.
%%
%% Although discouraged by the documentation on the RabbitMQ site, we re-use
%% internal functionality by way of the rabbit_router. This should be safe
%% as the implementation of the router has not changed in the last 5 years;
%% and even if it did it would be trivial to lift the logic out in an update
%% to this exchange (which would even prove backwards compatible). Due to the
%% requirement of both ETS and binding registration, it seems more logical to
%% re-use the existing tables to reduce memory and boilerplate.

-module(rabbit_exchange_type_delimiter).
-behaviour(rabbit_exchange_type).

-include_lib("rabbit_common/include/rabbit.hrl").

-export([description/0, serialise_events/0, route/2]).
-export([info/1, info/2, validate/1, validate_binding/2,
         create/2, delete/3, policy_changed/2, add_binding/3,
         remove_bindings/3, assert_args_equivalence/2]).

-rabbit_boot_step({?MODULE,
    [{description, "Exchange type to allow multiple routing keys on a message"},
     {mfa,         {rabbit_registry, register, [exchange, <<"x-delimiter">>, ?MODULE]}},
     {requires,    rabbit_registry},
     {enables,     kernel_ready}]}).

%%-----------------------------------------------
%% Implemented custom behaviour for this exchange
%%-----------------------------------------------

description() ->
    [{description, <<"AMQP direct-style exchange, allowing for multiple routing keys per message">>}].

route(#exchange{name = Name},
      % Walk the key list, splitting our delimited routes as necessary and then forward our new key
      % list through to the same implementation backing the direct exchange in order to gain
      % performance via ETS. We call rabbit_router directly to stay compatible with v3.11.x.
      #delivery{message = #basic_message{routing_keys = Routes}}) ->
    case split_routes(Routes, []) of
        []   -> [];
        Keys -> rabbit_router:match_routing_key(Name, Keys)
    end.

serialise_events() -> false.

%%----------------------------------------------------
%% Default behaviour implementations for this exchange
%%----------------------------------------------------

info(_X) -> [].
info(_X, _Is) -> [].
validate(_X) -> ok.
validate_binding(_X, _B) -> ok.
create(_Tx, _X) -> ok.
delete(_Tx, _X, _Bs) -> ok.
policy_changed(_X1, _X2) -> ok.
add_binding(_Tx, _X, _B) -> ok.
remove_bindings(_Tx, _X, _Bs) -> ok.
assert_args_equivalence(X, Args) ->
    rabbit_exchange:assert_args_equivalence(X, Args).

%%---------------------------------------------------
%% Private function implementations for this exchange
%%---------------------------------------------------

%% Simple splitting algorithm to walk the list of routing keys,
%% (usually of length <= 1), pull the delimiter back using the
%% first character in the key, then split the rest of the key
%% using the delimiter and add it to the buffer. Your key is
%% simply ignored in the case it's empty or malformed.
split_routes([], Routes) ->
    Routes;
split_routes([<<Delim:1/binary, Route/binary>> | RKeys], Routes) ->
    split_routes(RKeys, binary:split(Route, Delim, [global]) ++ Routes);
split_routes([_Route | RKeys], Routes) ->
    split_routes(RKeys, Routes).
