-module(comet_auth).
-behaviour(gen_server).

-export([start/0, stop/0, new/0, auth/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% External API
start() ->
	{ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
	Pid.

stop() ->
	gen_server:cast(?MODULE, stop).

new() ->
	gen_server:call(?MODULE, new).

release(Uuid, Sid) ->
	gen_server:cast(?MODULE, {{release, stop_queues}, Uuid, Sid}).

auth(Uuid, Sid, OSid) ->
	gen_server:call(?MODULE, {auth, Uuid, Sid, OSid}).

send_message(Pid, Uuid, Sid, Message) ->
	gen_server:cast(?MODULE, {message, Pid, Uuid, Sid, Message}).

%% Implementation
init([]) ->
	{ok, dict:new()}.

handle_call(new, _From, State) ->
	Uuid = hex_uuid(),
	Sid = comet_sid:get_sid(),

	DisconnectF = fun() ->
			% since this is going to be called from within comet_queue:handle_info
			% when a timeout happens, we don't wanna send a stop message from there (deadlocks?)
			gen_server:cast(?MODULE, {{release, dont_stop_queues}, Uuid, Sid})
	end,

	{reply, {ok, Uuid, Sid}, dict:store(Sid, {Uuid, none, false, comet_queue:start(Sid, DisconnectF)}, State)};

handle_call({auth, Uuid, Sid, OSid}, _From, State) ->
	case dict:find(Sid, State) of

		% try to find a Sid, Uuid match
		{ok, {Uuid, _, false, Queue}} -> case dict:find(OSid, State) of

				% look for an osid record
				{ok, {OUuid, Sid, false, OQueue}} ->
					% if osid record has our sid, authenticate
					{reply, ok, dict:store(OSid, {OUuid, Sid, true, OQueue}, dict:store(Sid, {Uuid, OSid, true, Queue}, State))};

				% no osid record or already authenticated
				_ -> {reply, {error, invalid_osid}, dict:store(Sid, {Uuid, OSid, false, Queue}, State)}
			end;

		% incorrect Sid, Uuid or already authenticated
		{ok, {Uuid, _, true, _}} -> {reply, {error, alreadyauth}, State};
		{ok, _} -> {reply, {error, invalid_uuid}, State};
		error -> {reply, {error, invalid_sid}, State}
	end.

handle_cast({message, Uuid, Sid, Message}, State) ->
	case dict:find(Sid, State) of
		% find sid with matching uuid, then corresponding osid
		{ok, {Uuid, OSid, true, _}} -> case dict:find(OSid, State) of
				{ok, {OUuid, Sid, true, Queue}} -> comet_queue:send_message(Queue, Message);
				_ -> {noreply, State}
			end;
		_ -> {noreply, State}
	end;

handle_cast({subscribe, Pid, Uuid, Sid}, State) ->
	case dict:find(Sid, State) of
		{ok, {Uuid, _, _, Queue}} -> comet_queue:subscribe(Sid, Pid), {noreply, State};
		_ -> {noreply, State}
	end;

handle_cast({unsubscribe, Pid, Uuid, Sid}, State) ->
	case dict:find(Sid, State) of
		{ok, {Uuid, _, _, Queue}} -> comet_queue:unsubscribe(Sid, Pid), {noreply, State};
		_ -> {noreply, State}
	end;

handle_cast({{release, stop_queues}, Uuid, Sid}, State) ->
	case osid_for_sid(Uuid, Sid, State) of
		{Sid, Queue, none, none} -> comet_queue:stop(Sid), {noreply, dict:erase(Sid)};
		{Sid, Queue, OSid, OQueue} ->
			comet_queue:stop(Sid),
			comet_queue:stop(OSid),
			{noreply, dict:erase(Sid, dict:erase(OSid))}
	end;

handle_cast({{release, dont_stop_queues}, Uuid, Sid}, State) ->
	case osid_for_sid(Uuid, Sid, State) of
		{Sid, Queue, none, none} -> {noreply, dict:erase(Sid)};
		{Sid, Queue, OSid, OQueue} -> {noreply, dict:erase(Sid, dict:erase(OSid))}
	end;

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info(_Message, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

%% Internal API
hex_uuid() -> os:cmd("uuidgen").

osid_for_sid(Uuid, Sid, State) ->
	case dict:find(Sid, State) of
		{ok, {Uuid, none, _, Queue}} -> {Sid, Queue, none, none};
		{ok, {Uuid, OSid, _, Queue}} -> case dict:find(OSid, State) of
				{ok, {OUuid, Sid, _, OQueue}} -> {Sid, Queue, OSid, OQueue};
				_ -> {Sid, Queue, none, none}
			end
	end.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

auth_test() ->
	comet_auth:start(),
	comet_sid:start(),

	{ok, Uuid1, Sid1} = comet_auth:new(),
	{ok, Uuid2, Sid2} = comet_auth:new(),

	?assertMatch({error, invalid_osid}, comet_auth:auth(Uuid1, Sid1, Sid2)),
	?assertMatch(ok, comet_auth:auth(Uuid2, Sid2, Sid1)),

	comet_sid:stop(),
	comet_auth:stop().

-endif.