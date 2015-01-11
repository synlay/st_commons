%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2015, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 11. Jan 2015 18:43
%%%-------------------------------------------------------------------
-module(libmisc_os).
-author("David Robakowski").

%% API
-export([
     retry_cmd_on_error/2
    ,retry_cmd_on_error/3
]).

%%--------------------------------------------------------------------
%% Func: retry_cmd_on_error(Cmd :: string(), MaxTries :: integer())
%%                                         -> ok | {error, max_tries}.
%%
%% Description: Tries to execute the Cmd argument with the os:cmd/1
%%              function and inspects the return code of the command.
%%              The function will try to execute the command MaxTries
%%              times with a default timeout of one second until
%%              another return code than 2 (error) is returned.
%%
%% Return:      'ok' if the return code is unqual to 2 (error),
%%              otherwise {error, 'max_tries'}.
%%--------------------------------------------------------------------
-spec retry_cmd_on_error(Cmd :: string(), MaxTries :: integer()) -> ok | {error, max_tries}.
retry_cmd_on_error(Cmd, MaxTries) ->
    retry_cmd_on_error(Cmd, MaxTries, 1000).

%%--------------------------------------------------------------------
%% Func: retry_cmd_on_error(Cmd :: string(), MaxTries :: integer(),
%%                          Timeout :: timeout()) -> ok
%%                                                 | {error, max_tries}.
%%
%% Description: like @link(retry_cmd_on_error/2) without the fix
%%              timeout.
%%
%% Return:      'ok' if the return code is unqual to 2 (error),
%%              otherwise {error, 'max_tries'}.
%%--------------------------------------------------------------------
-spec retry_cmd_on_error(Cmd :: string(), MaxTries :: integer(), Timeout :: timeout()) -> ok | {error, max_tries}.
retry_cmd_on_error(Cmd, MaxTries, Timeout) ->
    retry_cmd_on_error_helper(Cmd ++ "; echo $?", MaxTries, Timeout).

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

retry_cmd_on_error_helper(Cmd, Times, Timeout) when Times > 0 ->
    case lists:suffix("2\n", catch os:cmd(Cmd)) of
        false ->
            ok;
        true ->
            timer:sleep(Timeout),
            retry_cmd_on_error_helper(Cmd, Times - 1, Timeout)
    end;
retry_cmd_on_error_helper(_Cmd, _Times, _Timeout) ->
    {error, max_tries}.
