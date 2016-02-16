%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2015, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 14. Jan 2015 19:47
%%%-------------------------------------------------------------------
-module(st_lists_lib).
-author("David Robakowski").

%% API
-export([
    filterzipwith/3
    , is_empty/1
]).

-spec filterzipwith(Combine, List1, List2) -> List3 when
    Combine :: fun((X, Y) -> false | {true, T}),
    List1 :: [X],
    List2 :: [Y],
    List3 :: [T],
    X :: term(),
    Y :: term(),
    T :: term().
%% @doc Return [F(X0, Y0), F(X1, Y1), ..., F(Xn, Yn)] for lists [X0, X1, ..., Xn] and [Y0, Y1, ..., Yn] without
%%      the elements for which F (Xi, Yj) returns false.
%%      NOTE: The function is not tail recursive!
filterzipwith(F, [X | Xs], [Y | Ys]) ->
    case F(X, Y) of
        false ->
            filterzipwith(F, Xs, Ys);
        {true, Value} ->
            [Value | filterzipwith(F, Xs, Ys)]
    end;
filterzipwith(F, [], []) when is_function(F, 2) ->
    [].

-spec is_empty(List1) -> Result when
    List1 :: [X],
    X :: term(),
    Result :: boolean.
%% @doc Return true if list is empty.
is_empty([]) ->
    true;
is_empty(_) ->
    false.
