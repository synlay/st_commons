-ifndef(_common_constants_included).
-define(_common_constants_included, true).

-ifdef(debug).
    -define(TRACE(X), io:format("TRACE ~p:~p ~p~n", [?MODULE, ?LINE, X])).
-else.
    -define(TRACE(X), void).
-endif.

-define(ERR(Format, Data), error_logger:error_msg(Format, Data)).
-define(INFO(Format, Data), error_logger:info_msg(Format, Data)).

-define(GEN_SERVER_ATTRIB, [{debug, [log]}]).
                    
-endif.