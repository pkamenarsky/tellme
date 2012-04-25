-module(comet_queue).
-behaviour(gen_server).

-export([start/0, stop/0, send_message/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% External API
start() ->
	{ok, Pid} = gen_server:start({local, ?MODULE}, ?MODULE, [], []),
	Pid.

stop() ->
	gen_server:cast(?MODULE, stop).

send_message(Message) ->
	gen_server:handle_cast(?MODULE, {send_message, Message}).

%% Implementation
init([]) ->
	% pid, messages, tref
	{ok, {none, [], none}}.

handle_call({auth, Uuid, Sid, OSid}, _From, State) ->
	{reply, ok, State}.

handle_cast({send_message, Message}, State) ->
	case State of ->
			{none, Messages, TRef} -> {none, [Message | Messages], TRef};
			{Pid, Messages, TRef} -> Pid | Message

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

-endif.
