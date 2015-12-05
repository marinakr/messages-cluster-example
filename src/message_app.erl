-module(message_app).

-behaviour(application).

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start() ->
	start(default_type, []).

start(_StartType, _StartArgs) ->
	io:format("app starts~n"),
	Dispatch = cowboy_router:compile([
									  {'_', [
											 {"/", index_handler, []},
											 {'_', req_handler, []}
											]}
									 ]),
	Port = 8008,
	{ok, _} = cowboy:start_http(http_listener, 100,
								[{port, Port}],
								[{env, [{dispatch, Dispatch}]}]
							   ),
    message_sup:start_link().

stop(_State) ->
    ok.
