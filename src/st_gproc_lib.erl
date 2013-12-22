%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2013, Synlay Technologies UG (haftungsbeschraenkt) & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 18. Dec 2013 11:55
%%%-------------------------------------------------------------------
-module(st_gproc_lib).
-author("David Robakowski").

-type key()   :: any().
-type value() :: any().

-export_type([
     key/0
    ,value/0
]).

%% API
-export([
     reg_update_property/3
    ,reg_get_property/2
    ,reg_delete_property/2
    ,reg_set_value/2
    ,reg_lookup_value/1
    ,reg_create_counter/2
    ,reg_delete_counter/1
    ,reg_incr_counter/1
    ,reg_decr_counter/1
    ,reg_lookup_counter/1
]).

-spec reg_update_property(Key, PropertyKey, PropertyValue) -> ok | {error, Reason} when
    Key           :: key(),
    PropertyKey   :: atom(),
    PropertyValue :: value(),
    Reason        :: term().
reg_update_property(Key, PropertyKey, PropertyValue) ->
    Properties =
    case reg_lookup_value(Key) of
        {ok, Props} -> Props;
        {error, _Reason} -> []
    end,
    NewProperties = lists:keystore(PropertyKey, 1, Properties, {PropertyKey, PropertyValue}),
    reg_set_value(Key, NewProperties).

-spec reg_get_property(Key, PropertyKey) -> {ok, PropertyValue} | {error, Reason} when
    Key           :: key(),
    PropertyKey   :: atom(),
    PropertyValue :: value(),
    Reason        :: term().
reg_get_property(Key, PropertyKey) ->
    case reg_lookup_value(Key) of
        {ok, Props} ->
            case proplists:get_value(PropertyKey, Props) of
                undefined -> {error, not_found};
                Value -> {ok, Value}
            end;
        {error, _Reason}=Err -> Err
    end.

-spec reg_delete_property(Key, PropertyKey) -> ok | {error, Reason} when
    Key           :: key(),
    PropertyKey   :: atom(),
    Reason        :: term().
reg_delete_property(Key, PropertyKey) ->
    Properties =
    case reg_lookup_value(Key) of
        {ok, Props} -> Props;
        {error, _Reason} -> []
    end,
    reg_set_value(Key, proplists:delete(PropertyKey, Properties)).

-spec reg_set_value(Key, Value) -> ok | {error, Reason} when
    Key    :: key(),
    Value  :: value(),
    Reason :: term().
reg_set_value(Key, Value) ->
    case catch gproc:set_value_shared({p, l, Key}, Value) of
        {'EXIT', _Reason} ->
            %% no property is set because the key is not yet registered
            case catch gproc:reg_shared({p, l, Key}, Value) of
                {'EXIT', Error} ->
                    {error, Error};
                true ->
                    ok
            end;
        true ->
            ok
    end.

-spec reg_lookup_value(Key) -> {ok, Value} | {error, Reason} when
    Key    :: key(),
    Value  :: value(),
    Reason :: term().
reg_lookup_value(Key) ->
    case catch gproc:get_value_shared({p, l, Key}) of
        {'EXIT', _Reason} ->
            {error, not_found};
        Value ->
            {ok, Value}
    end.

-spec reg_create_counter(Key, Initial) -> ok | {error, Reason} when
    Key     :: key(),
    Initial :: integer(),
    Reason  :: term().
reg_create_counter(Key, Initial) ->
    case catch gproc:add_shared_local_counter(Key, Initial) of
        {'EXIT', _Reason} ->
            {error, already_existing};
        true ->
            ok
    end.

-spec reg_delete_counter(Key) -> ok | {error, Reason} when
    Key     :: key,
    Reason  :: term().
reg_delete_counter(Key) ->
    case catch gproc:unreg_shared({c, l, Key}) of
        {'EXIT', _Reason} ->
            {error, non_existing};
        true ->
            ok
    end.

-spec reg_incr_counter(Key) -> {ok, Counter} | {error, Reason} when
    Key     :: key(),
    Counter :: integer(),
    Reason  :: term().
reg_incr_counter(Key) ->
    case catch gproc:update_shared_counter({c, l, Key}, 1) of
        {'EXIT', _Reason} ->
            {error, non_existing};
        Counter ->
            {ok, Counter}
    end.

-spec reg_decr_counter(Key) -> {ok, Counter} | {error, Reason} when
    Key     :: key(),
    Counter :: integer(),
    Reason  :: term().
reg_decr_counter(Key) ->
    case catch gproc:update_shared_counter({c, l, Key}, -1) of
        {'EXIT', _Reason} ->
            {error, non_existing};
        Counter ->
            {ok, Counter}
    end.

-spec reg_lookup_counter(Key) -> {ok, Counter} | {error, Reason} when
    Key     :: key(),
    Counter :: integer(),
    Reason  :: term().
reg_lookup_counter(Key) ->
    case catch gproc:lookup_local_counters(Key) of
        {'EXIT', _Reason} ->
            {error, non_existing};
        [] ->
            {error, non_existing};
        Counters ->
            case proplists:get_value(shared, Counters) of
                undefined ->
                    {error, non_existing};
                Counter ->
                    {ok, Counter}
            end
    end.
