%%%-------------------------------------------------------------------
%%% @author David Robakowski
%%% @copyright (C) 2015, Synlay Technologies UG & Co. KG
%%% @doc
%%%
%%% @end
%%% Created : 21. Mar 2015 16:37
%%%-------------------------------------------------------------------
-module(st_supervisor_lib).
-author("David Robakowski").

-type max_restarts()            :: non_neg_integer().
-type max_sec_between_restart() :: pos_integer().

-type child_spec() :: {Id::id(), StartFunc::start_func(), Restart::restart(), Shutdown::shutdown(), Type::type(),
                       Modules::modules()}.

-type id() :: term().
-type supervisor_module() :: atom().
-type start_func() :: {M::atom(), F::atom(), A::[term()]}.
-type restart() :: permanent
                 | transient
                 | temporary.

-type shutdown() :: brutal_kill
                  | pos_integer()
                  | infinity.

-type type() :: worker
              | supervisor.

-type modules() :: [Module::atom()]
                 | dynamic.

%% API
-export_type([
     supervisor_module/0
    ,shutdown/0
    ,max_restarts/0
    ,max_sec_between_restart/0
]).

%% API
-export([
     new_child_spec/6
    ,new_convenient_child_spec/6
]).

-spec new_child_spec(Id, StartFunc, Restart, Shutdown, Type, Modules) -> ChildSpec when
    Id        :: id(),
    StartFunc :: start_func(),
    Restart   :: restart(),
    Shutdown  :: shutdown(),
    Type      :: type(),
    Modules   :: modules(),
    ChildSpec :: child_spec().
new_child_spec(Id, StartFunc, Restart, Shutdown, Type, Modules) ->
    {Id, StartFunc, Restart, Shutdown, Type, Modules}.

-spec new_convenient_child_spec(Id, Module, Args, Restart, Shutdown, Type) -> ChildSpec when
    Id        :: id(),
    Module    :: supervisor_module(),
    Args      :: list(any()),
    Restart   :: restart(),
    Shutdown  :: shutdown(),
    Type      :: type(),
    ChildSpec :: child_spec().
new_convenient_child_spec(Id, Module, Args, Restart, Shutdown, Type) ->
    new_child_spec(Id, {Module, start_link, Args}, Restart, Shutdown, Type, [Module]).
