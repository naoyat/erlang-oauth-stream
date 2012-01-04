-module(twitter).
-export([start/1, get/3, post/3, basic_auth_stream/5, oauth_stream/5]).

-include("twitter.hrl").

% erl -pa ebin -s crypto -s inets

loop(Username, Password, Consumer, AccessToken, AccessTokenSecret) ->
	receive
		{From, {get, URL, ExtraParams}} ->
			Response = oauth:get(URL, ExtraParams, Consumer, AccessToken, AccessTokenSecret),
			From ! { self(), Response },
			loop(Username, Password, Consumer, AccessToken, AccessTokenSecret);

		{From, {post, URL, ExtraParams}} ->
			Response = oauth:post(URL, ExtraParams, Consumer, AccessToken, AccessTokenSecret),
			From ! { self(), Response },
			loop(Username, Password, Consumer, AccessToken, AccessTokenSecret);

		{From, {oauth_stream, get, URL, ExtraParams, Processor}} ->
			Response = oauth_stream:get_stream(Processor, URL, ExtraParams, Consumer, AccessToken, AccessTokenSecret),
			From ! { self(), Response },
			loop(Username, Password, Consumer, AccessToken, AccessTokenSecret);

		{From, {oauth_stream, post, URL, ExtraParams, Processor}} ->
			Response = oauth_stream:post_stream(Processor, URL, ExtraParams, Consumer, AccessToken, AccessTokenSecret),
			From ! { self(), Response },
			loop(Username, Password, Consumer, AccessToken, AccessTokenSecret);

		{From, {basic_auth_stream, get, URL, ExtraParams, Processor}} ->
			Response = basic_auth_stream:get_stream(Processor, URL, ExtraParams, Username, Password),
			From ! { self(), Response },
			loop(Username, Password, Consumer, AccessToken, AccessTokenSecret);

		{From, {basic_auth_stream, post, URL, ExtraParams, Processor}} ->
			Response = basic_auth_stream:post_stream(Processor, URL, ExtraParams, Username, Password),
			From ! { self(), Response },
			loop(Username, Password, Consumer, AccessToken, AccessTokenSecret);

		{From, _} ->
			From ! { self(), {error, unknown_command} },
			loop(Username, Password, Consumer, AccessToken, AccessTokenSecret)
	end.

start(TwitterParams) ->
  #twitter_params{
    username = Username,
    password = Password,
    consumer_key = ConsumerKey,
    consumer_secret = ConsumerSecret,
    access_token = AccessToken,
    access_token_secret = AccessTokenSecret} = TwitterParams,

  Consumer = {ConsumerKey, ConsumerSecret, hmac_sha1},
% spawn(twitter, loop, [Consumer, AccessToken, AccessTokenSecret])
% io:format("twitter:loop(~p,~p,~p,~p,~p)..~n", [Username, Password, Consumer, AccessToken, AccessTokenSecret]),
  Pid = spawn(fun() -> loop(Username, Password, Consumer, AccessToken, AccessTokenSecret) end),
  io:format("Twitter Pid = ~p~n", [Pid]),
  Pid.

rpc(Pid, Request) ->
%  io:format("rpc sending request to ~p: ~p~n", [Pid, Request]),
  Pid ! {self(), Request},
  receive
    {Pid, Response} ->
%		  io:format("rpc received response from ~p: ~p~n", [Pid, Response]),
		  Response
  end.

get(Pid, URL, ExtraParams) ->
  case rpc(Pid, {get, URL, ExtraParams}) of
    {ok, Response} ->
      {_Protocol, _Header, Body} = Response,
      Body;
    Other ->
      {error, Other}
  end.

post(Pid, URL, ExtraParams) ->
  case rpc(Pid, {post, URL, ExtraParams}) of
    {ok, Response} ->
      {_Protocol, _Header, Body} = Response,
      Body;
    Other ->
      {error, Other}
  end.

basic_auth_stream(Pid, Method, URL, ExtraParams, Processor) ->
  rpc(Pid, {basic_auth_stream, Method, URL, ExtraParams, Processor}).

oauth_stream(Pid, Method, URL, ExtraParams, Processor) ->
  rpc(Pid, {oauth_stream, Method, URL, ExtraParams, Processor}).

