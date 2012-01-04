-module(basic_auth_stream).

-export([get_stream/5, post_stream/5]).
%-export([basic_auth_param/2]).

-import(streaming, [process_stream/2]).

basic_auth_param(Username, Password) ->
  {"Authorization", "Basic " ++ binary_to_list(base64:encode(Username ++ ":" ++ Password))}.

get_stream(Processor, URL, ExtraParams, Username, Password) ->
	BasicAuthParam = basic_auth_param(Username, Password),
	case ExtraParams of
		[] ->
			Request = { URL, [BasicAuthParam] };
		_  -> 
			ParamsEncoded = oauth:uri_params_encode(ExtraParams),
			Request = { URL ++ "?" ++ ParamsEncoded, [BasicAuthParam] }
	end,
	spawn(fun() ->
				  Response = httpc:request(get, Request, [{autoredirect, false}], [{sync, false}, {stream, self}]),
				  streaming:process_stream(Processor, Response)
		  end).

post_stream(Processor, URL, ExtraParams, Username, Password) ->
	BasicAuthParam = basic_auth_param(Username, Password),
	case ExtraParams of
		[] ->
			Request = { URL, [BasicAuthParam] };
		_ -> 
			ParamsEncoded = oauth:uri_params_encode(ExtraParams),
			Request = { URL, [BasicAuthParam], "application/x-www-form-urlencoded", ParamsEncoded }
	end,
	spawn(fun() ->
				  Response = httpc:request(post, Request, [{autoredirect, false}], [{sync, false}, {stream, self}]),
				  streaming:process_stream(Processor, Response)
		  end).
