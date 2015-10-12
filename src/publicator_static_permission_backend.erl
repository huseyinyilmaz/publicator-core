%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@HuseyinsMacBookPro.local>
%%% @copyright (C) 2015, Huseyin Yilmaz
%%% @doc
%%% Publicator static permission backend.
%%% @end
%%% Created : 11 Oct 2015 by Huseyin Yilmaz <huseyin@HuseyinsMacBookPro.local>
%%%-------------------------------------------------------------------
-module(publicator_static_permission_backend).

-behaviour(gen_server).
-behivour(pc_permission_backend).

%% API
-export([start_link/3]).
-export([has_permission/3]).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include("../include/publicator_core.hrl").

-record(filter, {consumer_code::binary()|all,
                 extra_data::list(),
                 channel_code::binary()|all,
                 permissions::#permissions{}}).

-record(state, {code::code(),
                args::any(),
                filters::[#filter{}]}).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link(code(), term(), message_meta()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Code, Args, Meta) ->
    gen_server:start_link(?MODULE, [Code, Args, Meta], []).


%%--------------------------------------------------------------------
%% @doc
%% gets permission for given user.
%% @end
%%--------------------------------------------------------------------
-spec has_permission(pid(), atom(), code()) -> boolean().
has_permission(Pid, Perm, Channel_code) ->
    {ok, Resp} = gen_server:call(Pid, {has_permission, Perm, Channel_code}),
    Resp,
    true.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Code, Args, _Meta]) ->
    Filters = [#filter{consumer_code=proplists:get_value(consumer_code, Arg, all),
                       channel_code=proplists:get_value(channel_code, Arg, all),
                       extra_data=proplists:get_value(extra_data, Arg, []),
                       permissions=#permissions{
                                      publish=proplists:get_value(publish, Arg, false),
                                      subscribe=proplists:get_value(subscribe_messages, Arg, false),
                                      create=proplists:get_value(create, Arg, false),
                                      listen_events=proplists:get_value(listen_events, Arg, false)}}
                   || Arg <- Args],

    Filters2 = lists:filter(
                     % keep only filters that is relative to current Consumer
                     fun(#filter{consumer_code=Filter_code}) ->
                             case Filter_code of
                                 all -> true;
                                 Code -> true;
                                 _ -> false
                             end
                     end,
                     Filters),

    % TODO add meta suport
    {ok, #state{code=Code, args=Args, filters=Filters2}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({has_permission, Perm, Channel_code}, _From,
            #state{filters=Filters}=State) ->
    Resp = lists:any(fun(Filter) ->
                             can_pass_filter(Filter, Perm)
                     end,
                     by_channel_code(Filters, Channel_code)),
    {reply, {ok, Resp}, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

by_channel_code(Filters, Channel_code) ->
    lists:filter(fun(#filter{channel_code=all}) -> true;
                    (#filter{channel_code=C}) when C==Channel_code -> true;
                    (_) -> false end,
                 Filters).

can_pass_filter(#filter{permissions=#permissions{
                                       publish=Filter_publish,
                                       subscribe=Filter_subscribe,
                                       listen_events=Filter_listen_events,
                                       create=Filter_create}},
                Permission)->
    case Permission of
        publish-> Filter_publish;
        subscribe->Filter_subscribe;
        listen_events->Filter_listen_events;
        create->Filter_create
    end.
