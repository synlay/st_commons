%%%-------------------------------------------------------------------
%%% @author Nils Ernsting
%%% @copyright (C) 2015, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 03. Mrz 2015 09:49
%%%-------------------------------------------------------------------
-module(st_traverse_utils_tests).
-author("Nils Ernsting").

-include_lib("eunit/include/eunit.hrl").

%%%============================================================================
%%% API
%%%============================================================================

gp_common_travserse_utils_test_() ->
    {foreach, fun setup/0, fun teardown/1,
        [
            fun get_element_tests/1
            , fun traverse_by_list_tests/1
            , fun traverse_by_path/1
            , fun key_exists/1
        ]}.

get_element_tests(_) ->
    [
        {"Result should be undefined", ?_assertEqual(undefined, st_traverse_utils:get_element(key, undefined))}
        , {"Result should be undefined", ?_assertEqual(undefined, st_traverse_utils:get_element(key, [{other_key, 1}]))}
        , {"Result should be 1", ?_assertEqual(1, st_traverse_utils:get_element(key, [{key, 1}]))}
        , {"Result should be 1", ?_assertEqual(1, st_traverse_utils:get_element(key, maps:from_list([{key, 1}])))}
    ].

traverse_by_list_tests(_) ->
    [
        {"Result should be undefined", ?_assertEqual(undefined, st_traverse_utils:traverse_by_list([key], undefined))}
        , {"Result should be undefined", ?_assertEqual(undefined, st_traverse_utils:traverse_by_list([key], [{other_key, 1}]))}
        , {"Result should be 1", ?_assertEqual(1, st_traverse_utils:traverse_by_list([key], [{key, 1}]))}
        , {"Result should be 1", ?_assertEqual(1, st_traverse_utils:traverse_by_list([key], maps:from_list([{key, 1}])))}
        , {"Result should be undefined", ?_assertEqual(undefined, st_traverse_utils:traverse_by_list([parent, key], maps:from_list([{key, 1}])))}
        , {"Result should be undefined", ?_assertEqual(undefined, st_traverse_utils:traverse_by_list([parent, key], [{parent, [{other_key, 1}]}]))}
        , {"Result should be undefined", ?_assertEqual(undefined, st_traverse_utils:traverse_by_list([parent, key], [{key, [{parent, 1}]}]))}
        , {"Result should be 1", ?_assertEqual(1, st_traverse_utils:traverse_by_list([parent, key], [{parent, [{key, 1}]}]))}
        , {"Result should be 1", ?_assertEqual(1, st_traverse_utils:traverse_by_list([parent, hd], [{parent, [1, 2, 3, 4]}]))}
        , {"Result should be 1", ?_assertEqual(1, st_traverse_utils:traverse_by_list([parent, <<"$1">>], [{parent, [1, 2, 3, 4]}]))}
        , {"Result should be 2", ?_assertEqual(2, st_traverse_utils:traverse_by_list([parent, <<"$2">>], [{parent, [1, 2, 3, 4]}]))}
        , {"Result should be 3", ?_assertEqual(3, st_traverse_utils:traverse_by_list([parent, <<"$3">>], [{parent, [1, 2, 3, 4]}]))}
        , {"Result should be undefined", ?_assertEqual(undefined, st_traverse_utils:traverse_by_list([parent, <<"$5">>], [{parent, [1, 2, 3, 4]}]))}
        , {"Result should be undefined", ?_assertEqual(undefined, st_traverse_utils:traverse_by_list([parent, <<"$-1">>], [{parent, [1, 2, 3, 4]}]))}
    ].

traverse_by_path(_) ->
    [
        {"Result should be undefined", ?_assertEqual(undefined, st_traverse_utils:traverse_by_path("key", undefined))}
        , {"Result should be undefined", ?_assertEqual(undefined, st_traverse_utils:traverse_by_path("key", [{<<"other_key">>, 1}]))}
        , {"Result should be 1", ?_assertEqual(1, st_traverse_utils:traverse_by_path("key", [{<<"key">>, 1}]))}
        , {"Result should be [1,2,3,4]", ?_assertEqual([1, 2, 3, 4], st_traverse_utils:traverse_by_path("parent", [{<<"parent">>, [1, 2, 3, 4]}]))}
        , {"Result should be 1", ?_assertEqual(1, st_traverse_utils:traverse_by_path("parent.$1", [{<<"parent">>, [1, 2, 3, 4]}]))}
        , {"Result should be 2", ?_assertEqual(2, st_traverse_utils:traverse_by_path("parent.$2", [{<<"parent">>, [1, 2, 3, 4]}]))}
        , {"Result should be 3", ?_assertEqual(3, st_traverse_utils:traverse_by_path("parent.$3", [{<<"parent">>, [1, 2, 3, 4]}]))}
    ].

key_exists(_) ->
    [
        {"Result should be false", ?_assertEqual(false, st_traverse_utils:key_exists(key, undefined))}
        , {"Result should be false", ?_assertEqual(false, st_traverse_utils:key_exists(key, [{other_key, 1}]))}
        , {"Result should be true", ?_assertEqual(true, st_traverse_utils:key_exists(key, [{key, 1}]))}
        , {"Result should be true", ?_assertEqual(true, st_traverse_utils:key_exists(key, maps:from_list([{key, 1}])))}
    ].

%%--------------------------------------------------------------------
%% @doc Setup each test set
%%--------------------------------------------------------------------
setup() ->
    ok.

teardown(_) ->
    ok.
