%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2015, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 05. Jul 2015 23:34
%%%-------------------------------------------------------------------
-module(st_inet_lib).
-author("David Robakowski").

-include("libmisc.hrl").

%% API
-export([
    peer_ip_string/1
]).

%% @doc Tries to parses an ip_address() and returns an IPv4 or IPv6 address string.
-spec peer_ip_string(PeerIp :: inet:ip_address()) -> PeerIpString :: string().
peer_ip_string(PeerIp) ->
    ?maybe_get_default(inet:ntoa(PeerIp), {error, einval}, lists:flatten(io_lib:format("~p", [PeerIp]))).
