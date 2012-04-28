-module(comet_sid).
-behaviour(gen_server).

-export([start/0, stop/0, get_sid/0, release_sid/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% External API
start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

get_sid() ->
	gen_server:call(?MODULE, get).

release_sid(Sid) ->
	gen_server:cast(?MODULE, {release, Sid}).

%% Implementation
init([]) ->
	{ok, {0, []}}.

handle_call(get, _From, State) ->
	case State of
		{Count, []} -> {reply, Count, {Count + 100, lists:seq(Count + 1, Count + 100)}};
		{Count, [Sid]} -> {reply, Sid, {Count, []}};
		{Count, [Sid | Sids]} -> {reply, Sid, {Count, Sids}}
	end.

handle_cast({release, Sid}, State) ->
	{Count, Sids} = State,
	{noreply, {Count, [Sid | Sids]}};

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info(_Message, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sid_test() ->
	comet_sid:start(),
	Sid = comet_sid:get_sid(),
	Sid2 = comet_sid:get_sid(),
	?assertEqual(Sid + 1, Sid2),
	comet_sid:stop().

release_test() ->
	comet_sid:start(),
	Sid = comet_sid:get_sid(),
	comet_sid:release_sid(Sid),
	Sid2 = comet_sid:get_sid(),
	?assertEqual(Sid, Sid2),
	comet_sid:stop().

-endif.
