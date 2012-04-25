-module(comet_auth).
-behaviour(gen_server).

-export([start/0, stop/0, new/2, auth/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% External API
start() ->
	{ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
	Pid.

stop() ->
	gen_server:cast(?MODULE, stop).

new(Uuid, Sid) ->
	gen_server:cast(?MODULE, {new, Uuid, Sid}).

release(Sid) ->
	gen_server:cast(?MODULE, {release, Sid}).

auth(Uuid, Sid, OSid) ->
	gen_server:call(?MODULE, {auth, Uuid, Sid, OSid}).

%% Implementation
init([]) ->
	{ok, dict:new()}.

handle_call({auth, Uuid, Sid, OSid}, _From, State) ->
	case dict:find(Sid, State) of
		{ok, {Uuid, _, false}} -> case dict:find(OSid, State) of
				{ok, {OUuid, Sid, false}} ->
					{reply, ok, dict:store(OSid, {OUuid, Sid, true}, dict:store(Sid, {Uuid, OSid, true}, State))};
				_ -> {reply, {error, invalid_osid}, dict:store(Sid, {Uuid, OSid, false}, State)}
			end;
		{ok, {Uuid, _, true}} -> {reply, {error, alreadyauth}, State};
		{ok, _} -> {reply, {error, invalid_uuid}, State};
		error -> {reply, {error, invalid_sid}, State}
	end.

handle_cast({new, Uuid, Sid}, State) ->
	% uuid, osid, authenticated
	{noreply,dict:store(Sid, {Uuid, none, false}, State)};

handle_cast({release, Sid}, State) ->
	% TODO
	{noreply, State};

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

auth_test() ->
	comet_auth:start(),
	comet_auth:new(3, 3),
	comet_auth:new(4, 4),

	?assertMatch({error, invalid_osid}, comet_auth:auth(3, 3, 4)),
	?assertMatch(ok, comet_auth:auth(4, 4, 3)).

-endif.
