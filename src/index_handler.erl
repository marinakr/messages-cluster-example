-module(index_handler).
-behaviour(cowboy_http_handler).
%% Cowboy_http_handler callbacks
-export([
		 init/2,
	handle/2,
	terminate/3
]).

init(Req, Opts) ->
    Req2 = cowboy_req:reply(200,
        [{<<"content-type">>, <<"text/plain">>}],
        <<"Hello Erlang!">>,
        Req),
    {ok, Req2, Opts}.


handle(Req, State) ->
	Body = <<"<h1>It works!</h1>">>,
	{ok, Req2} = cowboy_req:reply(200, [], Body, Req),
	{ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.

