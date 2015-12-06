%% @author marinn
%% @doc @todo Add description to mnesiadb.


-module(mnesiadb).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0]).

-record(message, {hashcode, value, exp_date, node}).


init() -> 	
	{ok, NodeList} = application:get_env(message,node_list),	
	Nodes = [H || {_, H} <-NodeList],	
	io:format("Start mnesia nodes:~n~p~n",[Nodes]),
	mnesia:create_schema(NodeList),
	rpc:multicall(Nodes, application, start, [mnesia]),
	mnesia:create_table(message,
						[{ram_copies, Nodes},
						 {attributes, record_info(fields, message)}]).

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
