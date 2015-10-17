%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@HuseyinsMacBookPro.local>
%%% @copyright (C) 2015, Huseyin Yilmaz
%%% @doc
%%% Tests for in memmory persistence backend.
%%% @end
%%% Created : 16 Oct 2015 by Huseyin Yilmaz <huseyin@Huseyins-air.home>
%%%-------------------------------------------------------------------
-module(inmemmory_persistence_backend_SUITE).

-compile(export_all).

-include("../include/publicator_core.hrl").

-include_lib("common_test/include/ct.hrl").

-define(META, #{}).
-define(DELAY, 500).

-define(PERSISTENCE_CONFIG,
        {publicator_inmemmory_persistence_backend, []}).

-define(LIMITED_PERSISTENCE_CONFIG,
        {publicator_inmemmory_persistence_backend,
         [[{channel_code, all}, {message_count, 2}]]}).

-define(PERMISSION_CONFIG,
        {publicator_static_permission_backend,
         [[{consumer_code, all},
           {extra_data, []},
           {channel_code, all},
           {publish, true},
           {subscribe, true},
           {create, true},
           {listen_events, true}]]}).

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

init_per_testcase(persist_message_limit, Config) ->
    lager:start(),
    pc_utils:set_env(publicator_core, permission_backend, ?PERMISSION_CONFIG),
    pc_utils:set_env(publicator_core, persistence_backend, ?LIMITED_PERSISTENCE_CONFIG),
    ok = publicator_core:start(),
    Config;

init_per_testcase(_TestCase, Config) ->
    lager:start(),
    pc_utils:set_env(publicator_core, permission_backend, ?PERMISSION_CONFIG),
    pc_utils:set_env(publicator_core, persistence_backend, ?PERSISTENCE_CONFIG),
    ok = publicator_core:start(),
    Config.

end_per_testcase(_TestCase, _Config) ->
%%    lager:info("Cleanup server"),
    ok = publicator_core:stop(),
    application:stop(lager),
    application:stop(goldrush),
    ok.

all() ->
    [get_backend_test_case,
     persist_message,
     persist_message_limit].

%%--------------------------------------------------------------------
%% @spec TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%% @end
%%--------------------------------------------------------------------
get_backend_test_case(_Config) ->
    Configuration = {publicator_inmemmory_persistence_backend,
                     [
                      [{channel_code, all}, {message_count, 20}]
                     ]},
    pc_utils:set_env(publicator_core, persistence_backend, Configuration),
    {Module, Args} = pc_persistence_backend:get_backend(),
    {Config_module, Config_args} = Configuration,
    true = ctcheck:equal({Module, Args},
                        {Config_module, Config_args ++ [?DEFAULT_PERSISTENCE_ARG]}),
    ok.

persist_message(_Config) ->
    {Module, Args} = pc_persistence_backend:get_backend(),
    Channel_code = <<"channel_code">>,
    {ok, Pid} = Module:start_link(Channel_code, Args),
    {ok, Producer_code, Producer_pid} = publicator_core:create_producer(?META),
    publicator_core:subscribe(Producer_code, Channel_code, ?META),
    true = ctcheck:is_pid(Pid),
    Msg1 = #message{
       producer_code= <<"producer_code">>,
       channel_code=Channel_code,
       type=message,
       data= <<"data">>,
       meta=#{}
      },
    Msg2 = #message{
       producer_code= <<"producer_code2">>,
       channel_code=Channel_code,
       type=message,
       data= <<"data2">>,
       meta=#{}
      },
    ok = Module:persist_message(Pid, Msg1),
    ok = Module:get_messages(Pid, Producer_pid),
    timer:sleep(?DELAY),
    true = ctcheck:equal(publicator_core:get_messages(Producer_code), {ok, [Msg1]}),
    ok = Module:persist_message(Pid, Msg2),
    ok = Module:get_messages(Pid, Producer_pid),
    timer:sleep(?DELAY),
    true = ctcheck:equal(publicator_core:get_messages(Producer_code), {ok, [Msg2, Msg1]}),
    ok.


persist_message_limit(_Config) ->
    {Module, Args} = pc_persistence_backend:get_backend(),
    Channel_code = <<"channel_code">>,
    {ok, Pid} = Module:start_link(Channel_code, Args),
    {ok, Producer_code, Producer_pid} = publicator_core:create_producer(?META),
    publicator_core:subscribe(Producer_code, Channel_code, ?META),
    true = ctcheck:is_pid(Pid),
    Msg1 = #message{
       producer_code= <<"producer_code">>,
       channel_code=Channel_code,
       type=message,
       data= <<"data">>,
       meta=#{}
      },
    Msg2 = #message{
       producer_code= <<"producer_code2">>,
       channel_code=Channel_code,
       type=message,
       data= <<"data2">>,
       meta=#{}
      },
    Msg3 = #message{
       producer_code= <<"producer_code3">>,
       channel_code=Channel_code,
       type=message,
       data= <<"data3">>,
       meta=#{}
      },
    ok = Module:persist_message(Pid, Msg1),
    ok = Module:get_messages(Pid, Producer_pid),
    timer:sleep(?DELAY),
    true = ctcheck:equal(publicator_core:get_messages(Producer_code), {ok, [Msg1]}),
    ok = Module:persist_message(Pid, Msg2),
    ok = Module:get_messages(Pid, Producer_pid),
    timer:sleep(?DELAY),
    true = ctcheck:equal(publicator_core:get_messages(Producer_code), {ok, [Msg2, Msg1]}),
    ok = Module:persist_message(Pid, Msg3),
    ok = Module:get_messages(Pid, Producer_pid),
    timer:sleep(?DELAY),
    true = ctcheck:equal(publicator_core:get_messages(Producer_code), {ok, [Msg3, Msg2]}),
    ok.
