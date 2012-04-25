-module(comet_queue).
-behaviour(gen_server).

-export([start/1, stop/1, send_message/2, subscribe/2, unsubscribe/1, state/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% External API
start(Name) ->
	{ok, _} = gen_server:start_link({global, Name}, ?MODULE, [], []),
	{global, Name}.

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
init([]) ->
	% pid, messages, tref
	{ok, {none, [], none}}.

handle_call(state, _From, State) ->
	{reply, State, State};

handle_call(_, _From, State) ->
	{reply, ok, State}.

handle_cast({send_message, Message}, State) ->
	case State of
		{none, Messages, TRef} -> {noreply, {none, [Message | Messages], TRef}};
		{Pid, _, TRef} -> Pid ! Message, {noreply, {none, [], TRef}}
	end;

handle_cast({subscribe, Pid}, State) ->
	NewState = case State of
		{none, _, _} -> State;
		{OldPid, OldMessages, OldTRef} -> OldPid ! {error, close}, {none, OldMessages, OldTRef}
	end,

	case NewState of
		{none, [Message | Messages], TRef} -> Pid ! Message, {noreply, {none, Messages, TRef}};
		{none, [], TRef} -> {noreply, {Pid, [], TRef}}
	end;

handle_cast(unsubscribe, State) ->
	case State of
		{_, Messages, TRef} -> {noreply, {none, Messages, TRef}}
	end;

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

subscribe_test() ->
	comet_queue:start(self()),
	comet_queue:subscribe(self(), self()),
	comet_queue:subscribe(self(), self()),

	receive
		{error, close} -> ?assertEqual(1, 1)
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

-endif.
