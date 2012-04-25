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

release(Uuid, Sid) ->
	gen_server:cast(?MODULE, {release, Uuid, Sid}).

auth(Uuid, Sid, OSid) ->
	gen_server:call(?MODULE, {auth, Uuid, Sid, OSid}).

send_message(Pid, Uuid, Sid, Message) ->
	gen_server:cast(?MODULE, {message, Pid, Uuid, Sid, Message}).

%% Implementation
init([]) ->
	{ok, dict:new()}.

handle_call({auth, Uuid, Sid, OSid}, _From, State) ->
	case dict:find(Sid, State) of

		% try to find a Sid, Uuid match
		{ok, {Uuid, _, false, Data}} -> case dict:find(OSid, State) of

				% look for an osid record
				{ok, {OUuid, Sid, false, OData}} ->
					% if osid record has our sid, authenticate
					{reply, ok, dict:store(OSid, {OUuid, Sid, true, OData}, dict:store(Sid, {Uuid, OSid, true, Data}, State))};

				% no osid record or already authenticated
				_ -> {reply, {error, invalid_osid}, dict:store(Sid, {Uuid, OSid, false, Data}, State)}
			end;

		% incorrect Sid, Uuid or already authenticated
		{ok, {Uuid, _, true, _}} -> {reply, {error, alreadyauth}, State};
		{ok, _} -> {reply, {error, invalid_uuid}, State};
		error -> {reply, {error, invalid_sid}, State}
	end.

handle_cast({message, Pid, Uuid, Sid, Message}, State) ->
	case dict:find(Sid, State) of

		% find sid with matching uuid
		{ok, {Uuid, OSid, true, _}} -> case dict:find(OSid, State) of

				% in case there's already an open connection, just send off the message
				{ok, {OUuid, Sid, true, {OPid, _, OTRef}}} ->
					OPid ! Message,
					{noreply, dict:store(OSid, {OUuid, Sid, true, {none, [], OTRef}}, State)};

				% else queue up in internal message queue
				{ok, {OUuid, Sid, true, {none, OMessages, OTRef}}} ->
					{noreply, dict:store(OSid, {OUuid, Sid, true, {none, [Message | OMessages], OTRef}}, State)};

				% ignore
				_ -> {noreply, State}
			end;
		_ -> {noreply, State}
	end;

handle_cast({new, Uuid, Sid}, State) ->
	% uuid, osid, authenticated, {messages, tref}
				{noreply, dict:store(Sid, {Uuid, none, false, {}}, State)};

handle_cast({release, Uuid, Sid}, State) ->
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
