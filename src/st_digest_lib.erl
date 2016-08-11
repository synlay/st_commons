%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2015, Synlay Technologies UG & Co. KG
%%% @doc
%%% Library to convert hash digest to different textual
%%% representations.
%%% Functions are partly based on Adam Lindbergs post from:
%%% http://stackoverflow.com/questions/6818214/erlang-and-javascript-md5-digest-match
%%% @end
%%% Created : 29. Jun 2015 12:21
%%%-------------------------------------------------------------------
-module(st_digest_lib).
-author("David Robakowski").

%% API
-export([
     hexbinarystring/1
    ,hexbinarystring/2
    ,hexstring/1
    ,hexstring/2
]).

-spec hexbinarystring(BinaryDigest :: binary()) -> BinaryString :: binary().
hexbinarystring(BinaryDigest) ->
    hexbinarystring(BinaryDigest, false).

-spec hexbinarystring(BinaryDigest :: binary(), UpperCase :: boolean()) -> BinaryString :: binary().
hexbinarystring(BinaryDigest, false) when is_binary(BinaryDigest) ->
    << << (list_to_binary(io_lib:format("~2.16.0b", [C])))/binary >> || <<C>> <= BinaryDigest >>;
hexbinarystring(BinaryDigest, true) when is_binary(BinaryDigest) ->
    << << (list_to_binary(io_lib:format("~2.16.0B", [C])))/binary >> || <<C>> <= BinaryDigest >>.

-spec hexstring(BinaryDigest :: binary()) -> DigestString :: string().
hexstring(BinaryDigest) ->
    hexstring(BinaryDigest, false).

-spec hexstring(BinaryDigest :: binary(), UpperCase :: boolean()) -> DigestString :: string().
hexstring(BinaryDigest, false) when is_binary(BinaryDigest) ->
    lists:flatten([io_lib:format("~2.16.0b", [B]) || <<B>> <= BinaryDigest]);
hexstring(BinaryDigest, true) when is_binary(BinaryDigest) ->
    lists:flatten([io_lib:format("~2.16.0B", [B]) || <<B>> <= BinaryDigest]).
