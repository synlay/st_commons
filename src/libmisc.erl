-module(libmisc).
-vsn("1.0.0").

-type epoch_timestamp() :: integer().

-export_type([
    epoch_timestamp/0
]).

-export([
     hex_to_bin/1
	,app_properties/1
	,get_optional/3
	,get_required/2
    ,epoch_timestamp/0
]).

-include("libmisc.hrl").

-spec hex_to_bin(string()) -> binary().
hex_to_bin(Str) ->
    << << (erlang:list_to_integer([H], 16)):4 >> || H <- Str >>.

%%--------------------------------------------------------------------
%% Func: app_properties(FileName) -> term()
%%
%% Description: Tries to read the application properties from a file
%%              specified trough 'FileName'. The config file should
%%              be represented as a list of terms.
%%
%% Return:      The terms from the config file, otherwise an empty
%%              term => []
%%--------------------------------------------------------------------
app_properties(FileName) ->
    case file:consult(FileName) of
        {ok, Terms}     -> Terms;
        {error, Reason} -> io:format("error while trying to read the property file: '~p' reason: '~p'~n",[FileName, Reason]),
                           []
    end.

%%--------------------------------------------------------------------
%% Func: get_optional(AppEnv, Key, Default) -> term()
%%
%% Description: Reads the property by Key for a given environment
%%              specified trough 'AppEnv'.
%%
%% Return:      The terms from the environment, otherwise the default
%%              term specified trough 'Default'.
%%--------------------------------------------------------------------
get_optional(AppEnv, Key, Default) ->
    case application:get_env(AppEnv, Key) of
        undefined ->
            Default;
        {ok, Value} ->
            Value
    end.

%%--------------------------------------------------------------------
%% Func: get_required(AppEnv, Key) -> term()
%%
%% Description: Reads the property by Key for a given environment
%%              specified trough 'AppEnv'.
%%
%% Return:      The terms from the environment, otherwise throws an
%%              error with message {missing_config, Key}
%%--------------------------------------------------------------------
get_required(AppEnv, Key) ->
    case application:get_env(AppEnv, Key) of
        undefined ->
            throw({missing_config, Key});
        {ok, Value} ->
            Value
    end.

-spec epoch_timestamp() -> EpochTimestamp when
    EpochTimestamp :: epoch_timestamp().
epoch_timestamp() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()) - 62167219200.
