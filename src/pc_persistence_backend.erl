%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% Behivour that implements persistence backend behivour.
%%% Persistence backend is initialized per channel.
%%% @end
%%% Created : 18 Jan 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(pc_persistence_backend).

-include("../include/publicator_core.hrl").

-export([get_backend/0]).

%% We are not adding this because it will be added by gen_server behaviour already.
%% -callback start_link(Code::code(), Args::term()) -> {ok, pid()} | ignore | {error, any()}.

%%--------------------------------------------------------------------
%% @doc
%% Persist given message
%% @end
%%--------------------------------------------------------------------
-callback persist_message(Pid::pid(), Message::#message{}) -> ok.

-callback get_messages(Pid::pid(), Producer_pid::pid()) -> ok.

%%%===================================================================
%%% API
%%%===================================================================
-spec get_backend()->{Module::atom(), Args::term()}.
get_backend() ->
    {ok, {Module, Args}} = application:get_env(publicator_core, persistence_backend),
    {Module, Args ++ [?DEFAULT_PERSISTENCE_ARG]}.
