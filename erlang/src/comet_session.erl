-module(comet_session).

-export([start/0, stop/0, open_session/0, close_session/1, subscribe/2, unsubscribe/1, broadcast/2]).

-record(session, {subscribed = false, callback_pid = none, messages = []}).

%% External API
start() ->
	gen_store:start(?MODULE).

stop() ->
	gen_store:stop(?MODULE).

open_session() ->
	gen_store:insert(?MODULE, #session{}).

close_session(Id) ->
	gen_store:remove(?MODULE, Id).

update_session(Id, Subscribed, CallbackPid, Messages) ->
	gen_store:update(?MODULE, Id, #session{subscribed = Subscribed, callback_pid = CallbackPid, messages = Messages}).

subscribe(Id, CallbackPid) ->
	case  gen_store:find(?MODULE, Id) of

		{ok, #session{messages = Messages}} ->
			if
				length(Messages) /= 0 -> CallbackPid ! {messages, Messages};
				true -> ok
			end,
			update_session(Id, true, CallbackPid, []);

		_ ->
			throw({fail, session_not_found})
	end.

unsubscribe(Id) ->
	case  gen_store:find(?MODULE, Id) of
		{ok, _} ->
			update_session(Id, false, none, []);
		_ ->
			throw({fail, session_not_found})
	end.

broadcast(FromId, Message) ->
	case gen_store:find(?MODULE, FromId) of
		{ok, _} ->
			gen_store:map(?MODULE, fun

					%% Only forward/store message if ids differ
					(Id, Session) when FromId /= Id ->
						#session{subscribed = Subscribed, messages = Messages, callback_pid = CallbackPid} = Session,

						case Subscribed of
							true ->
								CallbackPid ! {messages, [Message]},
								#session{subscribed = false, callback_pid = none, messages = []};
							_ ->
								#session{subscribed = false, callback_pid = none, messages = [Message | Messages]}
						end;

					%% FromId == Id
					(_Id, Session) -> Session
				end),
			ok;
		_ ->
			throw({fail, session_not_found})
	end.


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

catchFail(Fun) ->
	try
		Fun(),
		?assertEqual("didn't fail", "")
	catch
		_Type:_What ->
			ok
	end.

invalid_session_test() ->
	comet_session:start(),

	catchFail(fun() -> comet_session:subscribe(0, self()) end),
	catchFail(fun() -> comet_session:unsubscribe(0) end),
	catchFail(fun() -> comet_session:broadcast(0, "message") end),

	comet_session:stop().

subscribe_queue_messages_test() ->
	comet_session:start(),

	S1 = comet_session:open_session(),
	S2 = comet_session:open_session(),

	comet_session:broadcast(S1, "message1"),
	comet_session:broadcast(S1, "message2"),
	comet_session:broadcast(S1, "message3"),

	comet_session:subscribe(S2, self()),

	receive
		{messages, Messages} -> ?assertEqual(["message3", "message2", "message1"], Messages)
	after 500 ->
			?assertEqual("no messages received", "")
	end,

	comet_session:unsubscribe(S2),
	comet_session:stop().

subscribe_direct_message_test() ->
	comet_session:start(),

	S1 = comet_session:open_session(),
	S2 = comet_session:open_session(),

	comet_session:subscribe(S2, self()),

	receive
		{messages, _} -> ?assertEqual("message received without having sent one", "")
	after 500 ->
			ok
	end,

	comet_session:broadcast(S1, "message1"),

	receive
		{messages, Messages} -> ?assertEqual(["message1"], Messages)
	after 500 ->
		?assertEqual("no message received", "")
	end,

	comet_session:unsubscribe(S2),
	comet_session:stop().

unsubscribe_direct_message_test() ->
	comet_session:start(),

	S1 = comet_session:open_session(),
	S2 = comet_session:open_session(),

	comet_session:subscribe(S2, self()),

	receive
		{messages, _} -> ?assertEqual("message received without having sent one", "")
	after 500 ->
			ok
	end,

	comet_session:unsubscribe(S2),
	comet_session:broadcast(S1, "message1"),

	receive
		{messages, _} -> ?assertEqual("message received after unsubscribing", "")
	after 500 ->
			ok
	end,

	comet_session:stop().

session_receiver() ->
	receive
		{messages, Messages} -> ?assertEqual(["message1"], Messages)
	after 500 ->
		?assertEqual("no message received", "")
	end,

	receive
		{messages, Messages2} -> ?assertEqual(["message2"], Messages2)
	after 500 ->
		?assertEqual("no message received", "")
	end.

broadcast_test() ->
	comet_session:start(),

	S1 = comet_session:open_session(),
	S2 = comet_session:open_session(),
	S3 = comet_session:open_session(),

	comet_session:broadcast(S1, "message1"),

	comet_session:subscribe(S2, spawn(fun session_receiver/0)),
	comet_session:subscribe(S3, spawn(fun session_receiver/0)),

	comet_session:broadcast(S1, "message2"),

	comet_session:unsubscribe(S2),
	comet_session:unsubscribe(S3),

	timer:sleep(1000),

	comet_session:stop().

-endif.
