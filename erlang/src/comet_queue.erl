-module(comet_queue).
-behaviour(gen_server).

-export([start/1, start/2, stop/1, send_message/2, subscribe/2, unsubscribe/1, state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-ifdef(TEST).
-define(DISCONNECT_INTERVAL, 100).
-else.
-define(DISCONNECT_INTERVAL, 30000).
-endif.

%% External API
start(Name) ->
	{ok, Pid} = gen_server:start_link({global, Name}, ?MODULE, [], []),
	{ok, Pid}.

start(Name, DisconnectF) ->
	{ok, Pid} = gen_server:start_link({global, Name}, ?MODULE, [DisconnectF], []),
	{ok, Pid}.

stop(Name) ->
	gen_server:cast({global, Name}, stop).

send_message(Name, Message) ->
	gen_server:cast({global, Name}, {send_message, Message}).

subscribe(Name, Pid) ->
	gen_server:cast({global, Name}, {subscribe, Pid}).

unsubscribe(Name) ->
	gen_server:cast({global, Name}, unsubscribe).

state(Name) ->
	gen_server:call({global, Name}, state).

%% Implementation
init(F) ->
	% pid, messages, tref
	case F of
		[DisconnectF] -> {ok, {none, [], DisconnectF}, ?DISCONNECT_INTERVAL};
		[] -> {ok, {none, [], none}, ?DISCONNECT_INTERVAL}
	end.

handle_call(state, _From, State) ->
	{reply, State, State};

handle_call(_, _From, State) ->
	{reply, ok, State}.

handle_cast({send_message, Message}, State) ->
	case State of
		{none, Messages, DisconnectF} -> {noreply, {none, [Message | Messages], DisconnectF}, ?DISCONNECT_INTERVAL};
		{Pid, _, DisconnectF} -> Pid ! Message, {noreply, {none, [], DisconnectF}, ?DISCONNECT_INTERVAL}
	end;

handle_cast({subscribe, Pid}, State) ->
	NewState = case State of
		{none, _, _} -> State;
		{OldPid, OldMessages, OldDisconnectF} -> OldPid ! {error, unsubscribe}, {none, OldMessages, OldDisconnectF}
	end,

	case NewState of
		{none, [Message | Messages], DisconnectF} -> Pid ! Message, {noreply, {none, Messages, DisconnectF}, ?DISCONNECT_INTERVAL};
		{none, [], DisconnectF} -> {noreply, {Pid, [], DisconnectF}, ?DISCONNECT_INTERVAL}
	end;

handle_cast(unsubscribe, State) ->
	case State of
		{_, Messages, DisconnectF} -> {noreply, {none, Messages, DisconnectF}, ?DISCONNECT_INTERVAL}
	end;

handle_cast(stop, State) ->
	{stop, normal, State}.

handle_info(timeout, State) ->
	case State of
		{_, _, none} -> {stop, normal, State};
		{_, _, DisconnectF} -> DisconnectF(), {stop, normal, State}
	end.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVersion, State, _Extra) ->
	{ok, State}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

subscribe_test() ->
	comet_queue:start(self()),
	comet_queue:subscribe(self(), self()),
	comet_queue:subscribe(self(), self()),

	receive
		{error, unsubscribe} -> ?assertEqual(1, 1)
	after 10 -> ?assertEqual("No close messages received", "")
	end,

	comet_queue:send_message(self(), message),
	receive
		message -> ?assertEqual(1, 1)
	after 10 -> ?assertEqual("No message received", "")
	end,

	comet_queue:stop(self()).

unsubscribe_test() ->
	comet_queue:start(self()),
	comet_queue:subscribe(self(), self()),
	comet_queue:unsubscribe(self()),

	comet_queue:send_message(self(), message),
	receive
		_ -> ?assertEqual("Unknown message received", 0)
	after 10 -> ?assertEqual(1, 1)
	end,

	comet_queue:send_message(self(), message),
	comet_queue:subscribe(self(), self()),
	receive
		message -> ?assertEqual(1, 1)
	after 10 -> ?assertEqual("No message received", "")
	end,

	comet_queue:stop(self()).

message_before_sub_test() ->
	comet_queue:start(self()),
	comet_queue:send_message(self(), message),
	comet_queue:subscribe(self(), self()),

	receive
		message -> ?assertEqual(1, 1)
	after 10 -> ?assertEqual("No message received", "")
	end,

	comet_queue:stop(self()).

message_after_sub_test() ->
	comet_queue:start(self()),
	comet_queue:subscribe(self(), self()),
	comet_queue:send_message(self(), message),

	receive
		message -> ?assertEqual(1, 1)
	after 10 -> ?assertEqual("No message received", "")
	end,

	comet_queue:send_message(self(), message),
	comet_queue:subscribe(self(), self()),

	receive
		message -> ?assertEqual(1, 1)
	after 10 -> ?assertEqual("No message received", "")
	end,

	comet_queue:stop(self()).

messages_test() ->
	comet_queue:start(self()),
	comet_queue:send_message(self(), message),
	comet_queue:send_message(self(), message),

	comet_queue:subscribe(self(), self()),

	receive
		message -> ?assertEqual(1, 1)
	after 10 -> ?assertEqual("No message received", "")
	end,

	comet_queue:subscribe(self(), self()),

	receive
		message -> ?assertEqual(1, 1)
	after 10 -> ?assertEqual("No message received", "")
	end,

	comet_queue:subscribe(self(), self()),

	receive
		_ -> ?assertEqual("Unknown message received", 0)
	after 10 -> ?assertEqual(1, 1)
	end,

	comet_queue:stop(self()).

timeout_test() ->
	{ok, Pid} = comet_queue:start(self()),

	{status, Pid, _, [_, _, _, _, [_, _, {data, [{_, State}]}]]} = sys:get_status(Pid),
	?assertMatch({none, [], none},State),
	
	% wait for timeout
	timer:sleep(200),

	?assertExit({noproc, {sys, get_status, [Pid]}}, sys:get_status(Pid)),

	comet_queue:stop(self()).

timeout_after_subscribe_test() ->
	{ok, Pid} = comet_queue:start(self()),

	comet_queue:send_message(self(), message),

	{status, Pid, _, [_, _, _, _, [_, _, {data, [{_, State}]}]]} = sys:get_status(Pid),
	?assertMatch({none, [message], none},State),

	comet_queue:subscribe(self(), self()),
	
	% wait for timeout
	timer:sleep(200),

	?assertExit({noproc, {sys, get_status, [Pid]}}, sys:get_status(Pid)),

	comet_queue:stop(self()).

disconnectF_test() ->
	Self = self(),
	{ok, Pid} = comet_queue:start(self(), fun() ->
			Self ! disconnect
			end),

	{status, Pid, _, [_, _, _, _, [_, _, {data, [{_, State}]}]]} = sys:get_status(Pid),
	
	% wait for timeout
	timer:sleep(200),

	receive
		disconnect -> ?assertEqual(1, 1)
	after 100 ->
			?assertEqual("disconnectF wasn't called", "")
	end,

	?assertExit({noproc, {sys, get_status, [Pid]}}, sys:get_status(Pid)),

	comet_queue:stop(self()).

-endif.
