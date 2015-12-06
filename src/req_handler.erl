-module(req_handler).
-behaviour(cowboy_http_handler).
%% Cowboy_http_handler callbacks
-export([
	init/2,
	handle/2,
	terminate/3
]).

-include("include/URLs.hrl").

init(Req, Opts) ->	
    handle(Req,Opts).

handle(Req, State) ->
	io:format("Req: ~p~n",[Req]),	 
	handle( cowboy_req:path(Req),cowboy_req:method(Req),Req,State).

handle(_, <<"GET">>, Req, State) ->
	Body = <<"<h1>This is a response for GET</h1>">>,
	{ok, Req3} = cowboy_req:reply(200, [], Body, Req),
	{ok, Req3, State};

handle(?ADD, <<"POST">>, Req, State) -> 
	{ok,Body,_} = cowboy_req:body(Req),
	io:format("~p~n",[Body]),	
	gen_server:call(message_srv, {add_node,Body}, 5000),
	cowboy_req:reply(200, [], <<"ok">>, Req),
    {ok, Req, State};

handle(?REMOVE, <<"POST">>, Req, State) -> 	
	{ok,Body,_} = cowboy_req:body(Req),
	io:format("~p~n",[Body]),	
	gen_server:call(message_srv, {remove_node,Body}, 5000),
	cowboy_req:reply(200, [],  <<"ok">>, Req),
    {ok, Req, State};

handle(Path = ?GET, <<"POST">>, Req, State) -> 
	Body = Path,
	cowboy_req:reply(200, [], Body, Req),
    {ok, Req, State};
handle(Path = ?SEND, <<"POST">>, Req, State) -> 
	Body = Path,
	cowboy_req:reply(200, [], Body, Req),
    {ok, Req, State};

handle(_, <<"POST">>, Req, State) -> 
	Body = <<"<h1>This is a response for POST</h1>">>,
	cowboy_req:reply(200, [], Body, Req),
    {ok, Req, State};

handle(_, _,  Req, State) ->
            Body = <<"<h1>This is a response for other methods</h1>">>,
            {ok, Req3} = cowboy_req:reply(200, [], Body, Req),
            {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.

