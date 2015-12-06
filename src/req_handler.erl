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
	io:format("~p~n~n~n",[Body]),	
	gen_server:call(message_srv, {remove_node,Body}, 5000),
	cowboy_req:reply(200, [],  <<"ok">>, Req),
    {ok, Req, State};

handle(?GET, <<"POST">>, Req, State) -> 
	{ok,Body,_} = cowboy_req:body(Req),
	J = gen_server:call(message_srv, {get,Body},5000),
	io:format("~nReply ~p~n",[J]),
	cowboy_req:reply(200, [], Body, Req),
    {ok, Req, State};
handle(?SEND, <<"POST">>, Req, State) -> 
	{ok,Body,_} = cowboy_req:body(Req),
	{Mega, Sec, Micro} = os:timestamp(),
	Hash = crypto:hash(md4,[
							erlang:integer_to_binary(Mega),
							erlang:integer_to_binary(Sec),
							erlang:integer_to_binary(Micro),
							Body]),
	io:format("Send message: ~p~n",[Body]),	
	message_srv ! {send,{Hash,Body}},
	cowboy_req:reply(200, [], Hash, Req),
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

