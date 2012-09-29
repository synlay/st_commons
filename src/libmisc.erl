-module(libmisc).
-vsn("1.0.0").

-export([hex_to_bin/1]).

-include("libmisc.hrl").

-spec ?MODULE:hex_to_bin(string()) -> binary().
hex_to_bin(Str) ->
    << << (erlang:list_to_integer([H], 16)):4 >> || H <- Str >>.
