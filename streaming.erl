-module(streaming).
-export([process_stream/2]).

process_stream(Processor, Response) ->
  case Response of
    {ok, RequestId} ->
      io:format("streaming:process_stream> ok, request id=~p~n", [RequestId]),
      process_stream_loop(RequestId, Processor);

    {error, Reason} ->
      io:format("streaming:process_stream> error, reason=~p~n", [Reason]),
      Processor ! {self(), {error, Reason}}
  end,
  Response.

% ↓id:takkkun さんの記事「Twitter Streaming APIをErlangから使ってみる」を参考にしました
% http://d.hatena.ne.jp/takkkun/20091016/1255722692

process_stream_loop(RequestId, Processor) ->
  receive
    {http, {RequestId, stream_start, Headers}} ->
      Processor ! {self(), {stream_start, Headers}},
      process_stream_loop(RequestId, Processor);

    {http, {RequestId, stream, Part}} ->
      case Part of
        <<"\r\n">> -> empty;
        _          -> Processor ! {self(), {stream, Part}}
      end,
      process_stream_loop(RequestId, Processor);

    {http, {RequestId, {error, Reason}}} ->
      Processor ! {self(), {error, Reason}};

    {From, stop} ->
      httpc:cancel_request(RequestId),
      From ! {self(), stopped};
    _ ->
      process_stream_loop(RequestId, Processor)
  end.
