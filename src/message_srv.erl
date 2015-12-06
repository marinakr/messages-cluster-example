-module(message_srv).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-record(state, {nodes = [],messages=[]}).
%-record(message, {hashcode, value, exp_date, node}).
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
    {ok, #state{}}.

handle_call({add_node,Node}, _From, State = #state{nodes = Nodes}) ->
	Re = Nodes++[erlang:binary_to_list(Node)],
	io:format("Node added, new list: ~n~p~n",[Re]),
	{reply, ok, State#state{nodes = Re}};

handle_call({remove_node,Node}, _From, State = #state{nodes = Nodes}) ->
	Re = Nodes--[erlang:binary_to_list(Node)],
	io:format("Node removed~n~p~n",[Re]),
	{reply, ok, State#state{nodes = Re}};

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
	%{Mega, Sec, Micro} = os:timestamp(),
	F = fun() ->
				mnesia:write(Message)%#message{exp_date = {Mega, Sec+5, Micro}})							
		end,
	erlang:send_after(5000, self(), {remove,Message}),
	mnesia:activity(transaction, F).

remove_message(Message = #message{}) ->	
	mnesia:transaction(fun() -> 
							   mnesia:delete_object(message, Message, write) 
					   end).

