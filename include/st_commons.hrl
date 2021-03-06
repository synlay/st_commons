-ifndef(_st_commonsincluded).
-define(_st_commonsincluded, true).

-include("st_commons_constants.hrl").

%-export([hex_to_bin/1]).

%-spec ?MODULE:hex_to_bin(string()) -> binary().

%% @doc Throws an exception with ThrowReason if the result of Expr doesn't equals
%%      Guard, otherwise the result will be returned.
%%      Guard must be any value that can be be used for pattern matching.
-define(throwIfNotEqual(Guard, Expr, ThrowReason), begin
                                                       ((fun() ->
                                                           case (Expr) of
                                                               Guard ->
                                                                   Guard;
                                                               _Other ->
                                                                   throw(ThrowReason)
                                                           end
                                                       end)())
                                                   end).

-define(maybe_get_default(Value, DefaultGuard, Default), begin
                                                             ((fun() ->
                                                                 case (Value) of
                                                                     DefaultGuard ->
                                                                         Default;
                                                                     Result ->
                                                                         Result
                                                                 end
                                                             end)())
                                                         end).

-endif.
