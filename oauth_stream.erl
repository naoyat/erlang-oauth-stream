-module(oauth_stream).
-export([get_stream/4, get_stream/6, get_stream/7,
         post_stream/4, post_stream/6, post_stream/7]).

get_stream(Processor, URL, ExtraParams, Consumer) ->
  get_stream(Processor, URL, ExtraParams, Consumer, "", "").

get_stream(Processor, URL, ExtraParams, Consumer, Token, TokenSecret) ->
  get_stream(Processor, URL, ExtraParams, Consumer, Token, TokenSecret, []).

get_stream(Processor, URL, ExtraParams, Consumer, Token, TokenSecret, HttpcOptions) ->
  spawn(fun() ->
          Response = oauth:get(URL, ExtraParams, Consumer, Token, TokenSecret, HttpcOptions ++ [{sync, false}, {stream, self}]),
          streaming:process_stream(Processor, Response)
        end).

post_stream(Processor, URL, ExtraParams, Consumer) ->
  post_stream(Processor, URL, ExtraParams, Consumer, "", "").

post_stream(Processor, URL, ExtraParams, Consumer, Token, TokenSecret) ->
  post_stream(Processor, URL, ExtraParams, Consumer, Token, TokenSecret, []).

post_stream(Processor, URL, ExtraParams, Consumer, Token, TokenSecret, HttpcOptions) ->
  spawn(fun() ->
          Response = oauth:post(URL, ExtraParams, Consumer, Token, TokenSecret, HttpcOptions ++ [{sync, false}, {stream, self}]),
          streaming:process_stream(Processor, Response)
        end).
