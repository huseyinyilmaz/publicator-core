
%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  6 Jul 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(publicator_core_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    %% Start main supervisor
    lager:info("Starting publicator_core"),
    lager:info("publicator_core settings:"),
    lager:info("~p", [application:get_all_env(publicator_core)]),

    case publicator_core_sup:start_link() of

	{ok, Pid} ->
            %% lager:debug("Get Values from environment variables"),
            
            %% {Module, Args} = pc_utils:get_env(server,
            %%                                  auth_backend,
            %%                                  ?DEFAULT_AUTH_BACKEND),
            
            %% lager:debug("Module=~p, Args=~p", [Module, Args]),
            %% server_sup:start_permanent_child(pc_auth_backend, [Module, Args]),
	    {ok, Pid};
	Error ->
	    Error
		end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
