%% @author marinn
%% @doc @todo Add description to mnesiadb.


-module(mnesiadb).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init/0]).

-record(message, {hashcode, value, exp_date}).

init() -> 
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(message,
						[{ram_copies, [node()]},
						 {attributes, record_info(fields, message)}]).

%% ====================================================================
%% Internal functions
%% ====================================================================

do_init_db_slave(MasterNode) ->
    mnesia:start(),
    mnesia:change_config(extra_db_nodes, [MasterNode]),
    mnesia:change_table_copy_type(schema, node(), disc_copies),
    Tabs = mnesia:system_info(tables) -- [schema],
    [mnesia:add_table_copy(Tab, node(), disc_copies) || Tab <- Tabs].
