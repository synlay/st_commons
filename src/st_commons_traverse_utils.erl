%%%-------------------------------------------------------------------
%%% @author Nils Ernsting
%%% @copyright (C) 2015, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 24. Mrz 2015 12:44
%%%-------------------------------------------------------------------
-module(st_commons_traverse_utils).
-author("Nils Ernsting").

-type field_name() :: string() | binary() | atom().

-type travserse_object() :: proplists:proplist() | map().

%% API
-export([
    traverse_by_list/2
    , get_element/2
    , traverse_by_path/2
    , key_exists/2
]).

-spec traverse_by_path(FieldPath, JSON) -> Value | undefined when
    FieldPath :: string() | binary(),
    JSON :: travserse_object(),
    Value :: any().
traverse_by_path(FieldPath, JSON) when is_binary(FieldPath) ->
    traverse_by_path(binary_to_list(FieldPath),JSON);
traverse_by_path(FieldPath, JSON) when is_list(FieldPath) ->
    Splits = re:split(FieldPath, "\\."),
    traverse_by_list(Splits, JSON).

-spec traverse_by_list(Keys, Object) -> Value | undefined when
    Keys :: [field_name()],
    Object :: travserse_object(),
    Value :: any().
traverse_by_list(_FieldNames, undefined) ->
    undefined;
traverse_by_list([], Object) ->
    Object;
traverse_by_list([hd | Path], Object) when is_list(Object) ->
    traverse_by_list(Path, hd(Object));
traverse_by_list([<<"$", Index/binary>> | Path], Object) when is_list(Object) ->
    traverse_by_list(Path, get_nth_element(binary_to_integer(Index), Object));
traverse_by_list([FieldName | Path], Object) ->
    traverse_by_list(Path, get_element(FieldName, Object)).

-spec key_exists(Key, Object) -> Result when
    Key :: field_name(),
    Object :: travserse_object(),
    Result :: boolean().
key_exists(FieldName, Object) ->
    case get_element(FieldName, Object) of
        undefined ->
            false;
        _ ->
            true
    end.

-spec get_element(Key, Object) -> Value | undefined when
    Key :: field_name(),
    Object :: travserse_object(),
    Value :: any().
get_element(_Key, undefined) ->
    undefined;
get_element(Key, Object) when is_list(Object) ->
    case lists:keyfind(Key, 1, Object) of
        {Key, Value} ->
            Value;
        false ->
            undefined
    end;
get_element(Key, Object) when is_map(Object) ->
    maps:get(Key, Object, undefined);
get_element(_Key, _Object) ->
    undefined.

%%%===================================================================
%%% Internal functions
%%%===================================================================

get_nth_element(_N, []) ->
    undefined;
get_nth_element(1, [H | _T]) ->
    H;
get_nth_element(N, [_H | T]) when N > 0 ->
    get_nth_element(N - 1, T);
get_nth_element(_N, _List) ->
    undefined.
