%% @author Mochi Media <dev@mochimedia.com>
%% @copyright comet Mochi Media <dev@mochimedia.com>

%% @doc Callbacks for the comet application.

-module(comet_app).
-author("Mochi Media <dev@mochimedia.com>").

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for comet.
start(_Type, _StartArgs) ->
    comet_deps:ensure(),
    comet_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for comet.
stop(_State) ->
    ok.
