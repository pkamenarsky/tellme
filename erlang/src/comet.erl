%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc comet.

-module(comet).
-author("Mochi Media <dev@mochimedia.com>").
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.


%% @spec start() -> ok
%% @doc Start the comet server.
start() ->
    comet_deps:ensure(),
    ensure_started(crypto),
    application:start(comet).


%% @spec stop() -> ok
%% @doc Stop the comet server.
stop() ->
    application:stop(comet).
