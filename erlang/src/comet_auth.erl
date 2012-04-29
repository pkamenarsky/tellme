-module(comet_auth).
-behaviour(gen_server).

-export([start/0, stop/0, new/0, release/2, auth/3, subscribe/3, unsubscribe/2, send_message/3, send_message_to_sid/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% External API
start() ->
	gen_server:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
	gen_server:cast(?MODULE, stop).

new() ->
	gen_server:call(?MODULE, new).

release(Uuid, Sid) ->
	gen_server:cast(?MODULE, {{release, stop_queues}, Uuid, Sid}).

auth(Uuid, Sid, OSid) ->
	gen_server:call(?MODULE, {auth, Uuid, Sid, OSid}).

subscribe(Pid, Uuid, Sid) ->
	gen_server:cast(?MODULE, {subscribe, Pid, Uuid, Sid}).

unsubscribe(Uuid, Sid) ->
	gen_server:cast(?MODULE, {unsubscribe, Uuid, Sid}).

send_message(Uuid, Sid, Message) ->
	gen_server:cast(?MODULE, {send_message, Uuid, Sid, Message}).

send_message_to_sid(Sid, Message) ->
	gen_server:cast(?MODULE, {send_message_to_sid, Sid, Message}).

%% Implementation
init([]) ->
	{ok, dict:new()}.

handle_call(new, _From, State) ->
	Uuid = list_to_binary(uuid:to_string(uuid:uuid4())),
	Sid = comet_sid:get_sid(),

	DisconnectF = fun() ->
			% since this is going to be called from within comet_queue:handle_info
			% when a timeout happens, we don't wanna send a stop message from there (deadlocks?)
			gen_server:cast(?MODULE, {{release, dont_stop_queues}, Uuid, Sid})
	end,

	Queue = Sid,
	comet_queue:start(Queue, DisconnectF),

	{reply, {ok, Uuid, Sid}, dict:store(Sid, {Uuid, none, false, Queue}, State)};

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

handle_cast({send_message, Uuid, Sid, Message}, State) ->
	case osid_for_sid(Uuid, Sid, State) of
		{_, _, none, none} -> {noreply, State};
		{_Sid, _Queue, _Osid, OQueue} -> comet_queue:send_message(OQueue, Message), {noreply, State}
	end;

handle_cast({send_message_to_sid, Sid, Message}, State) ->
	case dict:find(Sid, State) of
		{ok, {_, _, _, Queue}} -> comet_queue:send_message(Queue, Message), {noreply, State};
		_ -> {noreply, State}
	end;

handle_cast({subscribe, Pid, Uuid, Sid}, State) ->
	case dict:find(Sid, State) of
		{ok, {Uuid, _, _, Queue}} -> comet_queue:subscribe(Queue, Pid), {noreply, State};
		_ -> Pid ! {error, noauth}, {noreply, State}
	end;

handle_cast({unsubscribe, Uuid, Sid}, State) ->
	case dict:find(Sid, State) of
		{ok, {Uuid, _, _, Queue}} -> comet_queue:unsubscribe(Queue), {noreply, State};
		_ -> {noreply, State}
	end;

handle_cast({{release, stop_queues}, Uuid, Sid}, State) ->
	error_logger:info_msg("Releasing sid/stop: ~p~n", [Sid]),

	case osid_for_sid(Uuid, Sid, State) of
		{Sid, Queue, none, none} -> comet_queue:stop(Queue), {noreply, release_sid(Sid, Queue, State)};
		{Sid, Queue, OSid, OQueue} ->
			comet_queue:stop(Queue),
			comet_queue:stop(OQueue),
			{noreply, release_sid(Sid, Queue, release_sid(OSid, OQueue, State))};
		_ -> {noreply, State}
	end;

handle_cast({{release, dont_stop_queues}, Uuid, Sid}, State) ->
	error_logger:info_msg("Releasing sid/dontstop: ~p~n", [Sid]),

	case osid_for_sid(Uuid, Sid, State) of
		{Sid, Queue, none, none} -> {noreply, release_sid(Sid, Queue, State)};
		{Sid, Queue, OSid, OQueue} -> {noreply, release_sid(Sid, Queue, release_sid(OSid, OQueue, State))};
		_ -> {noreply, State}
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

release_sid(Sid, _Queue, State) ->
%	comet_queue:send_message(Queue, release),
	comet_sid:release_sid(Sid),
	dict:erase(Sid, State).

osid_for_sid(Uuid, Sid, State) ->
	case dict:find(Sid, State) of
		{ok, {Uuid, none, _, Queue}} -> {Sid, Queue, none, none};
		{ok, {Uuid, OSid, _, Queue}} -> case dict:find(OSid, State) of
				{ok, {_OUuid, Sid, _, OQueue}} -> {Sid, Queue, OSid, OQueue};
				_ -> {Sid, Queue, none, none}
			end;
		_ -> {none, none, none, none}
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

	comet_queue:stop(Sid1),
	comet_queue:stop(Sid2),

	comet_sid:stop(),
	comet_auth:stop().

timeout_test() ->
	{ok, Pid} = comet_auth:start(),
	comet_sid:start(),

	{ok, Uuid1, Sid1} = comet_auth:new(),
	{ok, Uuid2, Sid2} = comet_auth:new(),

	?assertMatch({error, invalid_osid}, comet_auth:auth(Uuid1, Sid1, Sid2)),
	?assertMatch(ok, comet_auth:auth(Uuid2, Sid2, Sid1)),

	timer:sleep(50),
	comet_auth:subscribe(self(), Uuid2, Sid2),
	timer:sleep(80),

	{status, Pid, _, [_, _, _, _, [_, _, {data, [{_, State}]}]]} = sys:get_status(Pid),

%	receive
%		release -> ?assertEqual(1, 1)
%	after 200 ->
%			?assertMatch("release message not received", "")
%	end,

	case dict:find(Sid1, State) of
		{ok, _} -> ?assertMatch("Sid not purged after timeout", "");
		_ -> ?assertEqual(1, 1)
	end,

	case dict:find(Sid2, State) of
		{ok, _} -> ?assertMatch("Sid not purged after timeout", "");
		_ -> ?assertEqual(1, 1)
	end,

	timer:sleep(100),

	comet_sid:stop(),
	comet_auth:stop().

message_test() ->
	{ok, Pid} = comet_auth:start(),
	comet_sid:start(),

	{ok, Uuid1, Sid1} = comet_auth:new(),
	{ok, Uuid2, Sid2} = comet_auth:new(),

	?assertMatch({error, invalid_osid}, comet_auth:auth(Uuid1, Sid1, Sid2)),
	?assertMatch(ok, comet_auth:auth(Uuid2, Sid2, Sid1)),

	comet_auth:subscribe(self(), Uuid2, Sid2),
	comet_auth:send_message(Uuid1, Sid1, some_message),

	receive
		some_message -> ?assertEqual(1, 1)
	after 50 ->
			?assertMatch("Message not received", "")
	end,

	comet_queue:stop(Sid1),
	comet_queue:stop(Sid2),

	comet_sid:stop(),
	comet_auth:stop().

-endif.
