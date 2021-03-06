%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@HuseyinsMacBookPro.local>
%%% @copyright (C) 2015, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2015 by Huseyin Yilmaz <huseyin@HuseyinsMacBookPro.local>
%%%-------------------------------------------------------------------
-module(static_permission_backend_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("../include/publicator_core.hrl").

-define(META, #{}).

%%--------------------------------------------------------------------
%% @spec suite() -> Info
%% Info = [tuple()]
%% @end
%%--------------------------------------------------------------------
suite() ->
    [{timetrap,{seconds,30}}].

%%--------------------------------------------------------------------
%% @spec init_per_suite(Config0) ->
%%     Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_suite(Config0) -> term() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_suite(_Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_group(GroupName, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_group(_GroupName, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_group(GroupName, Config0) ->
%%               term() | {save_config,Config1}
%% GroupName = atom()
%% Config0 = Config1 = [tuple()]
%% @end
%%--------------------------------------------------------------------
end_per_group(_GroupName, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec init_per_testcase(TestCase, Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
init_per_testcase(_TestCase, Config) ->
    Config.

%%--------------------------------------------------------------------
%% @spec end_per_testcase(TestCase, Config0) ->
%%               term() | {save_config,Config1} | {fail,Reason}
%% TestCase = atom()
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
end_per_testcase(_TestCase, _Config) ->
    ok.

%%--------------------------------------------------------------------
%% @spec groups() -> [Group]
%% Group = {GroupName,Properties,GroupsAndTestCases}
%% GroupName = atom()
%% Properties = [parallel | sequence | Shuffle | {RepeatType,N}]
%% GroupsAndTestCases = [Group | {group,GroupName} | TestCase]
%% TestCase = atom()
%% Shuffle = shuffle | {shuffle,{integer(),integer(),integer()}}
%% RepeatType = repeat | repeat_until_all_ok | repeat_until_all_fail |
%%              repeat_until_any_ok | repeat_until_any_fail
%% N = integer() | forever
%% @end
%%--------------------------------------------------------------------
groups() ->
    [].

%%--------------------------------------------------------------------
%% @spec all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%% @end
%%--------------------------------------------------------------------
all() ->
    [get_backend_test_case,
     valid_permission_test_case,
     invalid_permission_test_case,
     insufficent_permission_test_case].

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
    % Allow all permissions.
    Configuration = {publicator_static_permission_backend,
                     [[{producer_code, all},
                       {extra_data, []},
                       {channel_code, all},
                       {publish, true},
                       {subscribe, true},
                       {create, true},
                       {listen_events, true}]]},
    pc_utils:set_env(publicator_core, permission_backend, Configuration),
    % initialize backend
    {Module, Args} = pc_permission_backend:get_backend(),
    Configuration = {Module, Args},
    ok.

valid_permission_test_case(_Config) ->
    % Allow all permissions.
    Configuration = {publicator_static_permission_backend,
                     [[{producer_code, all},
                       {extra_data, []},
                       {channel_code, all},
                       {publish, true},
                       {subscribe, true},
                       {create, true},
                       {listen_events, true}]]},
    pc_utils:set_env(publicator_core, permission_backend, Configuration),
    % initialize backend
    {Module, Args} = pc_permission_backend:get_backend(),
    {ok, Pid} = Module:start_link(<<"Code">>, Args, ?META),
    true = Module:has_permission(Pid, publish, <<"channel1">>),
    ok.

invalid_permission_test_case(_Config) ->
    % add unsupported producer code.
    Configuration = {publicator_static_permission_backend,
                     [[{producer_code, <<"some other code">>},
                       {extra_data, []},
                       {channel_code, all},
                       {publish, true},
                       {subscribe, true},
                       {create, true},
                       {listen_events, true}]]},
    pc_utils:set_env(publicator_core, permission_backend, Configuration),
    % initialize backend
    {Module, Args} = pc_permission_backend:get_backend(),
    {ok, Pid} = Module:start_link(<<"code">>, Args, ?META),
    false = Module:has_permission(Pid, publish, <<"channel1">>),
    ok.

insufficent_permission_test_case(_Config) ->
    % do not add permission
    Configuration = {publicator_static_permission_backend,
                     [[{producer_code, all},
                       {extra_data, []},
                       {channel_code, all},
                       {publish, false},
                       {subscribe, true},
                       {create, true},
                       {listen_events, true}]]},
    pc_utils:set_env(publicator_core, permission_backend, Configuration),
    % initialize backend
    {Module, Args} = pc_permission_backend:get_backend(),
    {ok, Pid} = Module:start_link(<<"code">>, Args, ?META),
    true = ctcheck:equal(Module:has_permission(Pid, publish, <<"channel1">>), false),
    ok.
