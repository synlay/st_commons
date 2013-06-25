-ifndef(_common_types_included).
-define(_common_types_included, true).

-include("common_constants.hrl").

-type maybe(A) :: undefined  % <=> nothing, but more erlang like
                | {just, A}.
                
-type either(A, B) :: {left, A}
                    | {right, B}.
                    
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
-type proplist(Key, Val) :: [{Key, Val}].

-endif.