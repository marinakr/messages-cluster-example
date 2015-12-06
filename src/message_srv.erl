-module(message_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {nodes = [],mynode}).
%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_Args) ->
	{ok,{Host,Node}} = 
		 application:get_env(message,currentnode),
    {ok, #state{nodes = [Node],mynode = {Host,Node}}}.

handle_call({add_node,Node}, _From, State = #state{nodes = Nodes}) ->
	{T,Re} = add_new_node(Node,Nodes),
	case T of 
		null -> {reply, ok, State#state{nodes = Re}};
		_ -> 
			proc_lib:spawn(fun() -> 
								   [rpc:multicall([T], gen_server, call, 
												 [message_srv,{add_node_call,Host},
												  5000]) || Host <- Nodes -- [T]]
						    end),
			{reply, ok, State#state{nodes = Re}}
	end;
			  
handle_call({remove_node,Node}, _From, State = #state{nodes = Nodes}) ->
	{T,Re} = remove(Node,Nodes),
	case T of
		null -> {reply, ok, State#state{nodes = Re}};
		_ -> 
			proc_lib:spawn(fun() -> 
								   [rpc:multicall([T], 
												 gen_server, call, 
												 [message_srv,
												  {remove_node_call,Host},
												  5000]) || Host <- Nodes -- [T]]
						   end),
			{reply, ok, State#state{nodes = Re}}
	end;

handle_call({remove_node_call,Node},_From, State = #state{nodes = Nodes}) -> 	
	{_,Re} = remove(Node,Nodes),
	io:format("Node removed: ~p~nNew nodes: ~p",[Node,Re]),
	{noreply, State#state{nodes = Re}};

handle_call({add_node_call,Node}, _From, State = #state{nodes = Nodes}) -> 
	{_,Re} = add_new_node(Node, Nodes),
	{noreply, State#state{nodes = Re}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.
-record(message, {hashcode, value, exp_date, node}).


handle_info({send, Message}, State) -> 
	add_message(Message),
	{noreply, State};

handle_info({remove,Message}, State) -> 
	remove_message(Message),
	{noreply, State};

handle_info({get,Message}, State=#state{nodes = Nodes}) -> 
	get_message(Message,Nodes),
	{noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
add_message(Message = #message{}) ->
	F = fun() ->
				mnesia:write(Message) 							
		end,
	erlang:send_after(5000, self(), {remove,Message}),
	mnesia:activity(transaction, F).

remove_message(Message = #message{}) ->	
	mnesia:transaction(fun() -> 
							   mnesia:delete_object(message, Message, write) 
					   end).
get_message(Hash, AllowedNodes) ->
	F = fun() ->
				case mnesia:read({message, Hash}) of
					[#message{value=Val, node = Node}] ->
						Message = #message{hashcode = Hash, value = Val, node = Node},
						case lists:member(Node, AllowedNodes) of 
							true -> 
								self() ! {remove,Message},
								Message;
							_ ->
								Message#message{value = <<"Invalid message identificator">>}
						end;
					[] ->
						undefined
				end
		end,
	mnesia:activity(transaction, F).

remove(Node,Nodes)->
	{ok, NodeList} = application:get_env(message,node_list),
	T = proplists:get_value(Node,NodeList,null),
	Re = Nodes--[T],
	io:format("Node removed: ~p~n, new list: ~n~p~n",[T,Re]),
	{T,Re}.

add_new_node(Node, Nodes) ->
	{ok, NodeList} = application:get_env(message,node_list),
	case proplists:get_value(Node, NodeList, none) of 
		none -> 
			io:format("~ninvalid node"),
			{null,Nodes};
		ValNode -> 
			case lists:member(ValNode, Nodes) of
				true ->
					io:format("~nnode already exists"),
					{null,Nodes};
				_ ->
					Re = Nodes++[ValNode],
					io:format("Node added, new list: ~n~p~n",[Re]),
					{ValNode,Re}
			end
	end.
					  
					  
				




