%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2015, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 19. Jan 2015 18:03
%%%-------------------------------------------------------------------
-module(st_math_lib).
-author("David Robakowski").

%% API
-export([
     floor/1
    ,ceiling/1
]).

-spec floor(Number) -> integer() when
      Number :: number().
%% @doc floor/1 returns the largest integer not larger than x.
%% @see https://erlangcentral.org/wiki/index.php?title=Floating_Point_Rounding
floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) ->
    trunc(X).

-spec ceiling(Number) -> integer() when
      Number :: number().
%% @doc ceiling/1 returns the smallest integer not smaller than x.
%% @see https://erlangcentral.org/wiki/index.php?title=Floating_Point_Rounding
ceiling(X) when X < 0 ->
    trunc(X);
ceiling(X) ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T + 1
    end.
