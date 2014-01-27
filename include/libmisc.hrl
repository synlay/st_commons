-ifndef(_lib_miscincluded).
-define(_lib_miscincluded, true).

-include("common_types.hrl").

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

-endif.
