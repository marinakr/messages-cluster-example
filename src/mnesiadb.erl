%% @author marinn
%% @doc @todo Add description to mnesiadb.


-module(mnesiadb).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0]).

-record(message, {hashcode, value}).

init() -> 	
	{ok, NodeList} = application:get_env(message,node_list),	
	Nodes = [N || {_,N} <- NodeList],
	D = mnesia:delete_schema(Nodes),
	C = mnesia:create_schema(Nodes),
	S = mnesia:start(),
	T = mnesia:create_table(message,
						[{ram_copies, Nodes},
						 {type,set},
						 {record_name, message},
						 {attributes, record_info(fields, message)}]),
	io:format("Started: ~p  Clear: ~p Created: ~p Table: ~p~n",[S,D,C,T]),
	ok.

%% ====================================================================
%% Internal functions
%% ====================================================================
%% 
%% do_init_db_slave(MasterNode) ->
%%     mnesia:start(),
%%     mnesia:change_config(extra_db_nodes, [MasterNode]),
%%     mnesia:change_table_copy_type(schema, node(), disc_copies),
%%     Tabs = mnesia:system_info(tables) -- [schema],
%%     [mnesia:add_table_copy(Tab, node(), disc_copies) || Tab <- Tabs].
