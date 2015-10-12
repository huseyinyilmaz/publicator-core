%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% Behivour that implements permission backend behivour.
%%% @end
%%% Created : 18 Jan 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(pc_permission_backend).

-include("../include/publicator_core.hrl").

-export([get_backend/0]).


%% We are not adding this because it will be added by gen_server behaviour already.
%% -callback start_link(code(), term(), message_meta) -> {ok, pid()} | ignore | {error, any()}.

%%--------------------------------------------------------------------
%% @doc
%% Returns permissions for given Channel.
%% @end
%%--------------------------------------------------------------------
-callback has_permission(Permission::atom(), Channel_code::binary()) -> boolean().

%%%===================================================================
%%% API
%%%===================================================================
-spec get_backend()->{Module::atom(), Args::term()}.
get_backend() ->
    {ok, {Module, Args}} = application:get_env(publicator_core, permission_backend),
    {Module, Args}.
