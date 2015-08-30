%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%% Behivour that implements authentication backend behivour.
%%% @end
%%% Created : 18 Jan 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(pc_auth_backend).

-include("../include/publicator_core.hrl").

-export([get_authentication_backend/0]).

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-callback init_state(Auth_args::term()) -> New_state::term().



%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
-callback authenticate(Consumer_Code::binary(),
                       Meta::message_meta(),
                       State::term()) -> denied| granted.


%%%===================================================================
%%% API
%%%===================================================================
-spec get_authentication_backend()->{Module::atom(), Configuration::term()}.
get_authentication_backend() ->
    case application:get_env(publicator_core, auth_backend) of
        {ok, {Module, Args}} -> ok;
        %% if there is no configuration do not give any permission for anything.
        undefined -> {Module, Args} = {publicator_static_auth_backend, []}
    end,
    lager:warning("Module=~p, Args=~p", [Module, Args]),
    State = Module:init_state(Args),
    {Module, State}.
