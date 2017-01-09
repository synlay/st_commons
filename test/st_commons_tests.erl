-module(st_commons_tests).

-include_lib("eunit/include/eunit.hrl").

st_commons_test_() ->
    {setup,
        fun() ->
            ok
        end,
        fun(_) ->
            ok
        end,
        [
            {"st_commons_tests is alive",
                fun() ->
                    %% format is always: expected, actual
                    ?assert(true)
                end}
        ]}.
