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
     datetime_now/0
    ,datetime_older_than_sec/1
    ,epoch_timestamp/0
    ,epoch_timestamp/1
]).

-spec datetime_now() -> calendar:datetime().
datetime_now() ->
    calendar:universal_time().

-spec datetime_older_than_sec(Seconds) -> DateTime when
    Seconds  :: non_neg_integer(),
    DateTime :: calendar:datetime().
datetime_older_than_sec(Seconds) ->
    NowInSeconds = calendar:datetime_to_gregorian_seconds(datetime_now()),
    calendar:gregorian_seconds_to_datetime(NowInSeconds - Seconds).

-spec epoch_timestamp() -> EpochTimestamp when
    EpochTimestamp :: epoch_timestamp().
epoch_timestamp() ->
    epoch_timestamp(datetime_now()).

-spec epoch_timestamp(Now) -> EpochTimestamp when
    Now            :: calendar:datetime(),
    EpochTimestamp :: epoch_timestamp().
epoch_timestamp(Now) ->
    calendar:datetime_to_gregorian_seconds(Now) - 62167219200.
