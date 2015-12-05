-module(message).

%% API
-export([start/0, stop/0]).

-define(APPS, [crypto, ranch, cowlib, cowboy]).
%%%===================================================================
%%% API
%%%===================================================================

start() ->
	register(rcvr, self()),
	mnesiadb:init(),
	ok = ensure_started(?APPS),
	application:start(message).

stop() ->
	stop_apps(lists:reverse(?APPS)),
	application:stop(message).

%% ===================================================================
%% Internal functions
%% ===================================================================
ensure_started([]) -> ok;
ensure_started([App | Apps]) ->
	case application:start(App) of
		ok -> ensure_started(Apps);
		{error, {already_started, App}} -> ensure_started(Apps)
	end.

stop_apps([]) -> ok;
stop_apps([App | Apps]) ->
	application:stop(App),
	stop_apps(Apps).