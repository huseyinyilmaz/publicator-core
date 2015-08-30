%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 06 Jun 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(pc_producer_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).
-include("../include/publicator_core.hrl").

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts a new producer
%% @end
%%--------------------------------------------------------------------
-spec start_child(Meta::message_meta()) -> {ok, Code::binary(), Pid::pid()}
                                             | {error, permission_denied}.
start_child(Meta) ->
    {Auth_backend, Auth_state} = pc_auth_backend:get_authentication_backend(),
    Code = pc_utils:generate_code(),
    case Auth_backend:authenticate(Code, Meta, Auth_state) of
        false ->
            lager:info("Permission denied for code=~p", [Code]),
            {error, permission_denied};
        true ->
            lager:info("Starting a new producer with code ~p, Auth_info ~p and Auth_state ~p~n",
                       [Code, Meta, Auth_state]),
            {Permission_module, Permission_state} =
                pc_permission_backend:get_permission_backend(),
            Args_to_append = [Code, Permission_module, Permission_state],
            case supervisor:start_child(?SERVER, Args_to_append) of
                {ok, Pid} -> {ok, Code, Pid};
                {error, {already_exists, _Pid}} -> start_child(Meta)
            end
    end.
	     

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Restart = transient,
    Shutdown = 2000,
    Type = worker,

    Producer = {producer, {pc_producer, start_link, []},
	    Restart, Shutdown, Type, [pc_producer]},

    {ok, {SupFlags, [Producer]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================