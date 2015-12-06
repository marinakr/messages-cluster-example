-module(message_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {nodes = [],mynode}).
-record(message, {hashcode, value}).
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
    {ok, #state{nodes = [],mynode = {Host,Node}}}.

handle_call({add_node,Node}, _From, State = #state{nodes = Nodes}) ->
	{{T,V},Re} = add_new_node(Node,Nodes),
	case T of 
		null -> {reply, ok, State#state{nodes = Re}};
		_ -> 
			{ok, NodeList} = application:get_env(message,node_list),
			All = [H || {_,H} <- NodeList,H /= node()],
			proc_lib:spawn(fun() -> 
								   rpc:multicall(All, gen_server, call, 
												 [message_srv,
												  {add_node_call,V},
												  5000]) 
						   end),
			{reply, ok, State#state{nodes = Re}}
	end;
			  
handle_call({remove_node,Node}, _From, State = #state{nodes = Nodes}) ->
	{{T,V},Re} = remove(Node,Nodes),
	case T of
		null -> {reply, ok, State#state{nodes = Re}};
		_ -> 
			{ok, NodeList} = application:get_env(message,node_list),
			All = [H || {_,H} <- NodeList, H /= node()],
			proc_lib:spawn(fun() -> 
								   rpc:multicall(All, 
												 gen_server, call, 
												 [message_srv,
												  {remove_node_call,V},
												  5000])
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

handle_call({get,Message}, _From, State=#state{nodes = Nodes}) -> 
	io:format("ask message ~p~n",[Message]),
	{noreply, get_message(Message,Nodes),State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(stop, State) ->
	{stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.



handle_info({send, Message}, State) -> 
	add_message(Message),
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
add_message({Message,Value}) ->
	io:format("~nReceive message: ~p~n",[Message]),
	MessageDB = #message{hashcode = Message,value = Value},
	F = fun() ->
				mnesia:write(MessageDB) 							
		end,
	erlang:send_after(5000, self(), {remove,Message}),
	Re = mnesia:activity(transaction, F),
	io:format("Writed : ~p~n",[Re]),
	Re.

remove_message(Message = #message{}) ->	
	mnesia:transaction(fun() -> 
							   mnesia:delete_object(message, Message, write) 
					   end).
get_message(Hash, AllowedNodes) ->
	io:format("Hash, AllowedNodes: ~p~n",[[Hash, AllowedNodes]]),
	F = fun() ->
				case mnesia:read({message, Hash}) of
					[Message] ->
						io:format("~nMessage : ~p~n",[Message]),						
						Message;
%% 						case lists:member(Node, AllowedNodes) of 
%% 							true -> 
%% 								self() ! {remove,Message},
%% 								Message;
%% 							_ ->
%% 								Message#message{value = <<"Invalid message identificator">>}
%% 						end;
					[] ->
						undefined
				end
		end,
	Re = mnesia:activity(transaction, F),
	io:format("result: ~p",[Re]),
	Re.

remove(Node,Nodes)->
	{ok, NodeList} = application:get_env(message,node_list),
	T = proplists:get_value(Node,NodeList,null),
	Re = Nodes--[T],
	io:format("Node removed: ~p~n, new list: ~n~p~n",[T,Re]),
	{{T,Node},Re}.

add_new_node(Node, Nodes) ->
	{ok, NodeList} = application:get_env(message,node_list),
	case %lists:member(Node, [V || {H,V} <- NodeList]) of
		proplists:get_value(Node, NodeList, none) of 
		none -> 
			io:format("~ninvalid node ~n~p~n",[[Node,Nodes,NodeList]]),
			{null,Nodes};
		ValNode -> 
			case lists:member(ValNode, Nodes) of
				true ->
					io:format("~nnode already exists: ~p~n",[ValNode]),
					{null,Nodes};
				_ ->
					Re = Nodes++[ValNode],
					io:format("Node added, new list: ~n~p~n",[Re]),
					{{ValNode,Node},Re}
			end
	end.
					  
					  
				




