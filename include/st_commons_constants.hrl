-ifndef(_st_commons_constants_included).
-define(_st_commons_constants_included, true).

-ifdef(debug).
    -define(TRACE(X), io:format("TRACE ~p:~p ~p~n", [?MODULE, ?LINE, X])).
    -define(TRACEVAL(X), begin io:format("TRACE ~p:~p ~p~n", [?MODULE, ?LINE, X]), X end).
-else.
    -define(TRACE(X), void).
    -define(TRACEVAL(X), void).
-endif.

-define(ERR(Format, Data), error_logger:error_msg(Format, Data)).
-define(INFO(Format, Data), error_logger:info_msg(Format, Data)).

-define(GEN_SERVER_ATTRIB, [{debug, [log]}]).

-endif.
