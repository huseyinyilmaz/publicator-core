%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 11 Oct 2013 by Huseyin Yilmaz <huseyin@Huseyins-MacBook-Air.local>
%%%-------------------------------------------------------------------
-module(pc_channel).

-behaviour(gen_server).

%% API
-export([start_link/3]).
-export([publish/2]).
-export([get_producers/1]).
-export([add_producer/3]).
-export([remove_producer/2]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {code :: binary(),
		producer_table :: ets:tid(),
                cache_size :: number(),
                current_cache_size :: number(),
                cache :: queue:queue(binary()),
                timeout :: number()|infinity}).

-include("../include/publicator_core.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @end
%%--------------------------------------------------------------------
start_link(Code, Cache_size, Timeout) ->
    gen_server:start_link(?MODULE, [Code, Cache_size, Timeout], []).


publish(Channel_pid, Message) ->
    Response = gen_server:cast(Channel_pid, {publish, Message}),
    Response.



%spec get_producers(Channel_pid::binary()) -> {ok, [pid()]}.
get_producers(Channel_pid) ->
    {ok, Producer_list} = gen_server:call(Channel_pid, get_producers),
    {ok, Producer_list}.

add_producer(Channel_pid, Producer_pid, Producer_code) ->
    gen_server:call(Channel_pid, {add_producer, Producer_pid, Producer_code}).

remove_producer(Channel_pid, Producer_code) ->
    gen_server:call(Channel_pid, {remove_producer, Producer_code}).

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
%% if a channel with same name is already registered stop this server 
init([Code, Cache_size, Timeout]) ->
    Self = self(),
    case pc_global:get_or_register_channel(Code) of
	Self ->
            {ok, #state{code=Code,
			producer_table=ets:new(producer_table,[set, public]),
                        cache_size=Cache_size,
                        current_cache_size=0,
                        cache=queue:new(),
                        timeout=Timeout
                       }};
	Pid when is_pid(Pid) -> 
	    {stop, {already_started, Pid}}
    end.

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
handle_call({add_producer, Producer_pid, Producer_code}, _From,
	    #state{producer_table=Producer_table,
		   code=Channel_code,
                   timeout=Timeout}=State)->
    %% Send cache to newly subscribed producer
    %% XXX publish cached messages.

    %% gen_server:cast(self(), {send_cache_to_producer_pid, Producer_pid}),
    %% Add  Producer to new channel
    ets:insert(Producer_table,{Producer_code, Producer_pid}),
    lager:info("Producer ~p was subscribed to channel ~p",[Producer_code, Channel_code]),
    %% send subscribtio notification to producers that subscribed with 'all'' option
    Handler_list = ets:match(Producer_table, {'$1', '$2'}),
    lists:foldl(fun([C_code, C_pid], Acc) ->
			%% Do not sent message to newly subscribed producer
			case C_code of
			    Producer_code -> Acc;
			    _ ->pc_producer:push_message(C_pid,
                                                         #message{
                                                            producer_code=Producer_code,
                                                            channel_code=Channel_code,
                                                            type=add_subscribtion,
                                                            data= undefined,
                                                            meta=#{}}),
				Acc
			end
		end, ok, Handler_list),

    Reply = ok,
    
    {reply, Reply, State, Timeout};

%% remove producer handler producer and tell producer
%% to remove table from its own list
handle_call({remove_producer, Producer_code}, _From,
	    #state{producer_table=Producer_table,
		   code=Channel_code,
                   timeout=Timeout}=State)->

    ets:delete(Producer_table, Producer_code),
    ets:foldl(fun({_C_code, C_pid}, _Acc) ->
                        pc_producer:push_message(C_pid,
                                                 #message{
                                                    producer_code=Producer_code,
                                                    channel_code=Channel_code,
                                                    type=remove_subscribtion})
		end, ok, Producer_table),
    
    Reply = ok,
    {reply, Reply, State, Timeout};


handle_call(get_producers, _From,
	    #state{producer_table=Producer_table,
                   timeout=Timeout}=State)->
    Reply ={ok,
	    ets:foldl(fun({Key,_Value},Acc) -> [Key| Acc] end, [], Producer_table)},
    {reply, Reply, State, Timeout};
    


handle_call(Request, _From, #state{timeout=Timeout}=State) ->
    Reply = ok,
    lager:warning("Unhandled call data reached. ~p~n", [Request]),
    {reply, Reply, State, Timeout}.

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
handle_cast({publish, Message},
	    #state{producer_table=Producer_table,
                   cache_size=Cache_size,
                   current_cache_size=Current_cache_size,
                   cache=Cache,
                   timeout=Timeout}=State)->
    %% todo run this on another temprary process
    ets:foldl(fun({_Producer_Code, Producer_pid}, Acc) ->
		      pc_producer:push_message(Producer_pid, Message),
		      Acc
	      end, ok, Producer_table),
    if
        Cache_size =< Current_cache_size ->
            Cache1 = queue:drop(queue:in(Message,Cache)),
            Current_cache_size1 = Current_cache_size;
        Cache_size > Current_cache_size ->
            Cache1 = queue:in(Message,Cache),
            Current_cache_size1 = Current_cache_size + 1
    end,
    {noreply, State#state{current_cache_size=Current_cache_size1,
                          cache=Cache1}, Timeout};


handle_cast(Msg, #state{timeout=Timeout}=State) ->
    lager:warning("Unhandled cast message=~p", [Msg]),
    {noreply, State, Timeout}.

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
handle_info(timeout, #state{code=Code,
                            producer_table=Producer_table,
                            timeout=Timeout}=State)->
    lager:info("Channel ~p has timeout", [Code]),
    Producer_list = ets:match(Producer_table, {'$1', {'$2', '_'}}),
    case Producer_list of
	[] ->
	    lager:info("~p - All producers are dead. Channel is dying.", [Code]),
            {stop, normal , State};
	_ ->
            lager:info("~p - There is still alive producers. Channel will stay alive",
                       [Code]),
            {noreply, State, Timeout}
	end;

handle_info(Info, #state{code=Code,
                         timeout=Timeout}=State) ->
    lager:warning("Unhandled info message in channel ~p (~p)", [Code, Info]),
    {noreply, State, Timeout}.

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
