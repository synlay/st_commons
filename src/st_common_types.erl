%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2015, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 22. Mar 2015 10:45
%%%-------------------------------------------------------------------
-module(st_common_types).
-author("David Robakowski").

-type maybe(A) :: undefined  % <=> nothing, but more erlang like
                | {just, A}.

-type either(A, B) :: {left, A}
                    | {right, B}.

-ifdef(have_not_parameterized_dict_and_sets_support).

%% ------------------------------------
%% Type: dict
%% where:
%%    _K: a key of any type
%%    _V: a value of any type
%% description:
%%    polymorphic type wrapper for
%%    the type dict(), for better
%%    documentation purposes.
%% ------------------------------------
-type dict(_K,_V) :: dict().
%% ------------------------------------

-type set(_Val) :: set().

-else.

%% Erl >= 17 => maps support available
-type map_(Key, Value) :: #{Key => Value}.
-type dict(K,V) :: dict:dict(K, V).
-type set(Val) :: sets:set(Val).

-endif.

-type proplist(Key, Value) :: [{Key, Value}].
-type orddict(Key, Value)  :: [{Key, Value}].

-export_type([
     maybe/1
    ,either/2
    ,dict/2
    ,set/1
    ,proplist/2
    ,orddict/2
]).

-ifndef(have_not_parameterized_dict_and_sets_support).
-export_type([
    map_/2
]).
-endif.
