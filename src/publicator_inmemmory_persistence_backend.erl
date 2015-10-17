%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@HuseyinsMacBookPro.local>
%%% @copyright (C) 2015, Huseyin Yilmaz
%%% @doc
%%% Persistence backend that holds messages in memmory
%%% @end
%%% Created : 14 Oct 2015 by Huseyin Yilmaz <huseyin@HuseyinsMacBookPro.local>
%%%-------------------------------------------------------------------
-module(publicator_inmemmory_persistence_backend).

-behaviour(gen_server).
-behaviour(pc_persistence_backend).

%% API
-export([start_link/2]).
-export([get_messages/1]).
-export([persist_message/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-include("../include/publicator_core.hrl").

-record(state, {
          code::code(),
          cache_size::integer(),
          current_cache_size::integer(),
          cache::queue:queue(term())
         }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
-spec start_link(Code::code(), Args::term()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Code, Args) ->
    gen_server:start_link(?MODULE, [Code, Args], []).


-spec get_messages(pid()) -> [#message{}].
get_messages(Pid) ->
    {ok, Resp} = gen_server:call(Pid, get_messages),
    Resp.

-spec persist_message(pid(), #message{}) -> ok.
persist_message(Pid, Message) ->
    gen_server:cast(Pid, {persist_message, Message}).
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
init([Code, _Args]) ->
    {ok, #state{
            code=Code,
            cache_size=10,
            current_cache_size=0,
            cache=queue:new()}}.

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
handle_call(get_messages, _From, #state{cache=Message_cache}=State) ->
    lager:debug("#######################"),
    lager:debug("Get Cached Messages=~p", [queue:to_list(Message_cache)]),
    {reply, {ok, queue:to_list(Message_cache)}, State};

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
handle_cast({persist_message, Msg},
            #state{cache_size=Cache_size,
                   current_cache_size=Current_cache_size,
                   cache=Cache}=State) ->
    lager:debug("#######################"),
    lager:debug("Persist_message Msg=~p, State=~p", [Msg, State]),
    if
        Cache_size =< Current_cache_size ->
            Cache1 = queue:drop(queue:in(Msg, Cache)),
            Current_cache_size1 = Current_cache_size;
        Cache_size > Current_cache_size ->
            Cache1 = queue:in(Msg, Cache),
            Current_cache_size1 = Current_cache_size + 1
    end,
    {noreply, State#state{current_cache_size=Current_cache_size1,
                          cache=Cache1}};
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
