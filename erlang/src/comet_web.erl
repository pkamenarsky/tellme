%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for comet.

-module(comet_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2, hib_receive/4]).

-define(SOP_HEADERS, [{"Access-Control-Allow-Origin", "*"}]).

-ifdef(TEST).
-define(COMET_TIMEOUT, 200).
-else.
-define(COMET_TIMEOUT, 10000).
-endif.

%% External API

start(Options) ->
    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun(Req) -> ?MODULE:loop(Req, DocRoot) end,

    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
	"/" ++ Path = Req:get(path),
	try
		case Req:get(method) of
			% 'GET' ->
			%	Req:serve_file(Path, DocRoot);
			'POST' ->
				case Path of
					"channel" ->
						Command = jiffy:decode(Req:recv_body()),
						% error_logger:info_msg("Received command: ~p~n", [Command]),

						case Command of
							{[{<<"command">>, <<"get-uuid">>}]} ->
								{ok, Uuid, Sid} = comet_auth:new(),
								Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{uuid, Uuid}, {sid, Sid}]})});

							{[{<<"command">>, <<"auth">>}, {<<"uuid">>, Uuid}, {<<"sid">>, Sid}, {<<"osid">>, OSid}]} ->
								case comet_auth:auth(Uuid, Sid, OSid) of
									ok -> 
										comet_auth:send_message_to_sid(Sid, {ack, auth}),
										comet_auth:send_message_to_sid(OSid, {ack, auth}),

										Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{ack, ok}]})});
									{error, EReason} -> Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{ack, noauth}, {reason, EReason}]})})
								end;

							{[{<<"command">>, <<"message">>}, {<<"uuid">>, Uuid}, {<<"sid">>, Sid}, {<<"message">>, Message}]} ->
								comet_auth:send_message(Uuid, Sid, Message),
								Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{ack, ok}]})});

							{[{<<"command">>, <<"quote">>}, {<<"uuid">>, Uuid}, {<<"sid">>, Sid}, {<<"quotes">>, Quotes}]} ->
								comet_auth:send_message(Uuid, Sid, {quotes, Quotes}),
								Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{ack, ok}]})});

							_ -> Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{ack, error}, {reason, invalid}]})})
						end;

					"backchannel" ->
						Command = jiffy:decode(Req:recv_body()),
						case Command of

							{[{<<"uuid">>, Uuid}, {<<"sid">>, Sid}]} ->
								comet_auth:subscribe(self(), Uuid, Sid),

								% set active mode & hibernate process
								Socket = Req:get(socket),
								inet:setopts(Socket, [{active, once}]),

								TRef = erlang:send_after(?COMET_TIMEOUT, self(), {error, timeout}),
								proc_lib:hibernate(?MODULE, hib_receive, [Req, Uuid, Sid, TRef]);

							_ -> Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{ack, error}, {reason, invalid}]})})
						end;
					% path catch all
					_ ->
						Req:not_found()
				end;
			% method catch all
			_ ->
				Req:respond({501, [], []})
		end
	catch
		throw:{fail, _Reason} ->
			Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{ack, error}, {reason, internal}]})});
		Type:What ->
			Report = ["web request failed",
					{path, Path},
					{type, Type}, {what, What},
					{trace, erlang:get_stacktrace()}],
			error_logger:error_report(Report),
			Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{ack, error}, {reason, internal}]})})
	end.

%% Internal API

% as per https://groups.google.com/forum/?fromgroups#!topic/mochiweb/O5K3RsYiyXw
hib_receive(Req, Uuid, Sid, TRef) ->
	erlang:cancel_timer(TRef),

	receive
		% from socket
		{tcp_closed, _Socket} ->
			comet_auth:send_message(Uuid, Sid, {error, unsubscribe}),
			comet_auth:release(Uuid, Sid),
			Req:cleanup();
		{error, timeout} ->
			comet_auth:unsubscribe(Uuid, Sid),
			Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{ack, reconnect}]})});

		% from comet_queue
		{error, unsubscribe} ->
			Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{ack, close}]})});

		% from here
		{ack, auth} ->
			Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{ack, auth}]})});

		% from comet_auth
		{error, noauth} ->
			Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{ack, error}, {reason, noauth}]})});
		{quotes, Quotes} ->
			Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{ack, quote}, {quotes, Quotes}]})});
		Message ->
			Req:ok({"text/plain", ?SOP_HEADERS, jiffy:encode({[{ack, message}, {message, Message}]})})
	end.

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

%%
%% Tests
%%
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

catch_fail(Fun) ->
	try
		Fun(),
		?assertEqual("didn't fail", "")
	catch
		_Type:_What ->
			ok
	end.

get_body(Url, Command) ->
	Response = ibrowse:send_req("http://localhost:8080/" ++ Url, [], post, jiffy:encode({Command})),
	% error_logger:info_msg("Body: ~p~n", [Response]),
	case Response of
		{ok, _, _, Body} ->
			jiffy:decode(Body);
		_ -> ?assertEqual(true, false)
	end.

get_uuid_test() ->
	ibrowse:start(),
	Response = get_body("channel", [{command, <<"get-uuid">>}]),
	?assertMatch({[{<<"uuid">>, _}, {<<"sid">>, _}]}, Response),
	ibrowse:stop().

auth_test() ->
	ibrowse:start(),
	{[{<<"uuid">>, Uuid1}, {<<"sid">>, Sid1}]} = get_body("channel", [{command, <<"get-uuid">>}]),
	{[{<<"uuid">>, Uuid2}, {<<"sid">>, Sid2}]} = get_body("channel", [{command, <<"get-uuid">>}]),

	{[{<<"ack">>, <<"noauth">>}, {<<"reason">>, _}]} = get_body("channel", [{command, <<"auth">>}, {uuid, Uuid1}, {sid, Sid1}, {osid, Sid2}]),
	{[{<<"ack">>, <<"ok">>}]} = get_body("channel", [{command, <<"auth">>}, {uuid, Uuid2}, {sid, Sid2}, {osid, Sid1}]),

	{[{<<"ack">>, <<"auth">>}]} = get_body("backchannel", [{uuid, Uuid1}, {sid, Sid1}]),
	{[{<<"ack">>, <<"auth">>}]} = get_body("backchannel", [{uuid, Uuid2}, {sid, Sid2}]),

	ibrowse:stop().

backchannel_noauth_test() ->
	ibrowse:start(),

	{[{<<"ack">>, <<"error">>}, {<<"reason">>, <<"noauth">>}]} = get_body("backchannel", [{uuid, 0}, {sid, 1}]),

	ibrowse:stop().

backchannel_test() ->
	ibrowse:start(),
	{[{<<"uuid">>, Uuid1}, {<<"sid">>, Sid1}]} = get_body("channel", [{command, <<"get-uuid">>}]),
	{[{<<"uuid">>, Uuid2}, {<<"sid">>, Sid2}]} = get_body("channel", [{command, <<"get-uuid">>}]),

	{[{<<"ack">>, <<"noauth">>}, {<<"reason">>, _}]} = get_body("channel", [{command, <<"auth">>}, {uuid, Uuid1}, {sid, Sid1}, {osid, Sid2}]),
	{[{<<"ack">>, <<"ok">>}]} = get_body("channel", [{command, <<"auth">>}, {uuid, Uuid2}, {sid, Sid2}, {osid, Sid1}]),

	{[{<<"ack">>, <<"auth">>}]} = get_body("backchannel", [{uuid, Uuid1}, {sid, Sid1}]),
	{[{<<"ack">>, <<"auth">>}]} = get_body("backchannel", [{uuid, Uuid2}, {sid, Sid2}]),

	{[{<<"ack">>, <<"ok">>}]} = get_body("channel", [{command, <<"message">>}, {uuid, Uuid2}, {sid, Sid2}, {message, "A message"}]),
	{[{<<"ack">>, <<"message">>}, {<<"message">>, "A message"}]} = get_body("backchannel", [{uuid, Uuid1}, {sid, Sid1}]),

	ibrowse:stop().

%backchannel_timeout_test() ->
%	ibrowse:start(),
%	{[{<<"uuid">>, Uuid1}, {<<"sid">>, Sid1}]} = get_body("channel", [{command, <<"get-uuid">>}]),
%	{[{<<"uuid">>, Uuid2}, {<<"sid">>, Sid2}]} = get_body("channel", [{command, <<"get-uuid">>}]),
%
%	{[{<<"ack">>, <<"error">>}, {<<"reason">>, _}]} = get_body("channel", [{command, <<"auth">>}, {uuid, Uuid1}, {sid, Sid1}, {osid, Sid2}]),
%	{[{<<"ack">>, <<"ok">>}]} = get_body("channel", [{command, <<"auth">>}, {uuid, Uuid2}, {sid, Sid2}, {osid, Sid1}]),
%
%	timer:sleep(200),
%	{[{<<"command">>, <<"reconnect">>}]} = get_body("backchannel", [{uuid, Uuid1}, {sid, Sid1}]),
%
%	ibrowse:stop().

-endif.
