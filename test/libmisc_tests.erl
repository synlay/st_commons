-module(libmisc_tests).

-include_lib("eunit/include/eunit.hrl").

libmisc_test_() ->
    {setup,
        fun() ->
            ok
        end,
        fun(_) ->
            ok
        end,
        [
            {"libmisc_tests is alive",
                fun() ->
                    %% format is always: expected, actual
                    ?assert(true)
                end}
        ]}.
