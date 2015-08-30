%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyins-air.home>
%%% @copyright (C) 2014, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 18 Jan 2014 by Huseyin Yilmaz <huseyin@huseyins-air.home>
%%%-------------------------------------------------------------------
-module(publicator_static_auth_backend).
-behivour(pc_auth_backend).
%% API
-export([init_state/1, authenticate/3]).

-include("../include/publicator_core.hrl").

-record(auth_filter, {consumer_code::binary()|all,
                      meta::list()|all}).

-record(state, {filter_list::list()}).

%%%===================================================================
%%% API
%%%===================================================================
-callback init_state(Auth_args::term()) -> New_state::term().
init_state(Auth_args) ->
    lager:warning("AUTH_args=~p", [Auth_args]),
    Auth_list = [#auth_filter{consumer_code=proplists:get_value(consumer_code, Auth_arg, all),
                              meta=proplists:get_value(meta, Auth_arg, [])
                             }
                 || Auth_arg <- Auth_args],
    lager:warning("AUTH_list=~p", [Auth_list]),

    #state{filter_list=Auth_list}.



%% -spec init([Args::term()]) -> {ok, State::term()}.
%% init([Args])->
%%     %% lager:debug("==================="),
%%     %% lager:debug("Start auth  backend"),
%%     lager:debug("///////////////////////////"),
%%     lager:debug("call s_authbackend:start_link/1 "),
%%     lager:debug("Args=~p~n", [Args]),
%%     Auth_list = [#auth_filter{consumer_code=proplists:get_value(consumer_code, Arg, all),
%%                               group=proplists:get_value(group, Arg, all),
%%                               auth_info=proplists:get_value(consumer_code, Arg, all)}
%%                  || Arg <- Args],
%%     {ok, #state{filter_list=Auth_list}}.


-spec authenticate(Consumer_code::binary(),
                   Meta::message_meta(),
                   State::term()) -> boolean().
authenticate(Consumer_code, Meta, #state{filter_list=Filter_list}=_State) ->
    lists:any(fun(Filter)->
                      can_authenticate(Filter,
                                       Consumer_code,
                                       Meta)
              end, Filter_list).

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Checks meta data passes for given consumer code.
%% TODO. we need to do actual check here.
-spec can_authenticate(tuple(), binary(), message_meta()) -> boolean().
can_authenticate(#auth_filter{consumer_code=Filter_consumer_code,
                              meta=_Filter_meta},
                 Consumer_code,
                 _Meta)->

    lists:all(fun({Value,Filter_value})->
                      pc_backend_utils:is_equal_or_all(Value,Filter_value)end,
              [{Consumer_code,Filter_consumer_code}]).
               %% {Meta, Filter_meta}]).
        %% and pc_backend_utils:is_extra_data_passes(Extra_data, Filter_extra_data).
