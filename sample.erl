-module(sample).
-export([start/0,
         stream_process_loop/1, react_to_status/2]).

-include("twitter.hrl").

start() ->
  % erl -pa ebin -s crypto -s inets
  crypto:start(),
  inets:start(),
  ssl:start(),

  TwitterPid = twitter:start(#twitter_params{
    username            = "USERNAME HERE",
    password            = "PASSWORD HERE",
    consumer_key        = "CONSUMER KEY HERE",
    consumer_secret     = "CONSUMER SECRET HERE",
    access_token        = "ACCESS TOKEN HERE",
    access_token_secret = "ACCESS TOKEN SECRET HERE"}),

  twitter_api:user_stream(TwitterPid, spawn(fun() -> stream_process_loop(TwitterPid) end)),
  ok.


stream_process_loop(TwitterPid) ->
  receive
	  {_Streamer, {start, _Headers}} ->
		  stream_process_loop(TwitterPid);

	  {_Streamer, {stream, Part}} ->
		  case mochijson2:decode(Part) of
			  {struct, [{<<"friends">>, Friends}]} -> io:format("friends: ~p~n", [Friends]);
			  {struct, Status}                     -> react_to_status(Status, TwitterPid);
			  Other                                -> io:format("ignored: ~p~n", [Other])
		  end,
		  stream_process_loop(TwitterPid);

	  {_Streamer, {error, Reason}} ->
		  io:format("process_loop()... <error> reason:~p~n", [Reason])
  end.

react_to_status(Status, TwitterPid) ->
  % Text           = proplists:get_value(<<"text">>, Status),
  StatusIdStr    = binary_to_list( proplists:get_value(<<"id_str">>, Status) ),
  {struct, User} = proplists:get_value(<<"user">>, Status),

  % ScreenName = proplists:get_value(<<"screen_name">>, User),
  UserIdStr  = binary_to_list( proplists:get_value(<<"id_str">>, User) ),

  Params = [{"status", "blah blah blah"},
            {"in_reply_to_status_id", StatusIdStr},
            {"in_reply_to_user_id", UserIdStr}],

  twitter_api:update(TwitterPid, Params).

