%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2015, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 30. Jun 2015 10:00
%%%-------------------------------------------------------------------
-module(st_calendar_lib).
-author("David Robakowski").

-type epoch_timestamp() :: non_neg_integer().

-export_type([
    epoch_timestamp/0
]).

-export([
     now_datetime/0
    ,epoch_timestamp/0
    ,epoch_timestamp/1
]).

-spec now_datetime() -> calendar:datetime().
now_datetime() ->
    calendar:universal_time().

-spec epoch_timestamp() -> EpochTimestamp when
    EpochTimestamp :: epoch_timestamp().
epoch_timestamp() ->
    epoch_timestamp(st_calendar_lib:now_datetime()).

-spec epoch_timestamp(Now) -> EpochTimestamp when
    Now            :: calendar:datetime(),
    EpochTimestamp :: epoch_timestamp().
epoch_timestamp(Now) ->
    calendar:datetime_to_gregorian_seconds(Now) - 62167219200.
