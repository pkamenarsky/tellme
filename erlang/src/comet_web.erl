%% @author Mochi Media <dev@mochimedia.com>
%% @copyright 2010 Mochi Media <dev@mochimedia.com>

%% @doc Web server for comet.

-module(comet_web).
-author("Mochi Media <dev@mochimedia.com>").

-export([start/1, stop/0, loop/2]).


%% External API

start(Options) ->
	% Session manager
	comet_sid:start(),

    {DocRoot, Options1} = get_option(docroot, Options),
    Loop = fun(Req) -> ?MODULE:loop(Req, DocRoot) end,

    mochiweb_http:start([{name, ?MODULE}, {loop, Loop} | Options1]).

stop() ->
    mochiweb_http:stop(?MODULE).

loop(Req, DocRoot) ->
	"/" ++ Path = Req:get(path),
	try
		case Req:get(method) of
			'GET' ->
				Parameters = Req:parse_qs(),
				Callback = proplists:get_value("callback", Parameters),

				case Path of
					"backchannel" ->
						receive
							{messages, Messages} ->
								Req:ok({"text/plain", jsonp_wrap(Callback, "ok")})
						after 10000 ->
								Req:ok({"text/plain", jsonp_wrap(Callback, "nothing")})
						end;
					_ ->
						Req:serve_file(Path, DocRoot)
				end;
			'POST' ->
				case Path of
					"channel" ->
						Command = jiffy:decode(Req:recv_body()),
						case Command of
							{[{<<"command">>, <<"get-uuid">>}]} -> Req:ok({"text/plain", jiffy:encode({[{sid, comet_sid:get_sid()}, {uuid, hex_uuid()}]})});
							_ -> Req:ok({"text/plain", ":("})
						end;
					_ ->
						Req:not_found()
				end;
			_ ->
				Req:respond({501, [], []})
		end
	catch
		throw:{fail, Reason} ->
			Req:ok({"text/plain", jsonp_wrap("fail", "{result: \"fail\", reason: \"" ++ to_string(Reason) ++ "\"}")});
		Type:What ->
			Report = ["web request failed",
					{path, Path},
					{type, Type}, {what, What},
					{trace, erlang:get_stacktrace()}],
			error_logger:error_report(Report),
			%% NOTE: mustache templates need \ because they are not awesome.
			Req:ok({"text/plain", "{result: \"fail\", reason: \"" ++ to_string(Report) ++ "\"}"})
			%           Req:respond({500, [{"Content-Type", "text/plain"}],
			%                        "request failed, sorry\n"})
	end.

%% Internal API
hex_uuid() -> os:cmd("uuidgen").

get_option(Option, Options) ->
    {proplists:get_value(Option, Options), proplists:delete(Option, Options)}.

jsonp_wrap(Callback, Json) ->
	case Callback of
		undefined -> Json;
		_ -> Callback ++ "('" ++ Json ++ "');"
	end.

string_format(Pattern, Values) ->
    lists:flatten(io_lib:format(Pattern, Values)).
    
to_string(Value) ->
	string_format("~p", [Value]).

to_integer(undefined) ->
	undefined;
	
to_integer(String) ->
	case string:to_integer(String) of
		{error, _} -> throw({fail, not_an_integer});
		{X, _} -> X
	end.

value_to_binary(Value) when is_list(Value) ->
	list_to_binary(Value);

value_to_binary(Value) ->
	Value.

value_or(Value, Alternative) when Value == undefined ->
	Alternative;

value_or(Value, _Alternative) ->
	Value.
	
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
	Response = ibrowse:send_req("http://localhost:8080/" ++ Url, [], post, jiffy:encode({[Command]})),
	case Response of
		{ok, _, _, Body} ->
			jiffy:decode(Body);
		_ -> ?assertEqual(true, false)
	end.

channel_test() ->
	ibrowse:start(),
	Uuid = get_body("channel", {command, <<"get-uuid">>}),
	?assertMatch({[{<<"sid">>, _}, {<<"uuid">>, _}]}, Uuid),
	ibrowse:stop().

-endif.
