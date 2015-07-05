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

-type peer_string() :: [byte()].

-export_type([
    peer_string/0
]).

%% API
-export([
     peer_ip_string/1
    ,normalize_peer_ip_string/1
    ,normalize_peer_ip_string/2
]).

%% @doc Tries to parses an ip_address() and returns an IPv4 or IPv6 address string.
-spec peer_ip_string(PeerIp :: inet:ip_address()) -> PeerIpString :: peer_string().
peer_ip_string(PeerIp) ->
    ?maybe_get_default(inet:ntoa(PeerIp), {error, einval}, lists:flatten(io_lib:format("~p", [PeerIp]))).

%% @doc Normailze a peers IP address. IPv6 for example could have small
%%      hex characters.
-spec normalize_peer_ip_string(PeerIpString :: peer_string()) -> NormalizedPeerIpString :: peer_string().
normalize_peer_ip_string(PeerIpString) ->
    normalize_peer_ip_string(PeerIpString, true).

-spec normalize_peer_ip_string(PeerIpString :: peer_string(), ToUpper :: boolean())
                                                                             -> NormalizedPeerIpString :: peer_string().
normalize_peer_ip_string(PeerIpString, false) ->
    string:to_lower(PeerIpString);
normalize_peer_ip_string(PeerIpString, true) ->
    string:to_upper(PeerIpString).
