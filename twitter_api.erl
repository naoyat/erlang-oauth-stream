-module(twitter_api).
-export([home_timeline/2, update/2, user_stream/2, sample/2, filter/3]).

%% standard
home_timeline(Pid, Params) ->
  twitter:get(Pid, "https://api.twitter.com/statuses/home_timeline.json", Params).

update(Pid, StatusText) when is_binary(StatusText) ->
  update(Pid, [{"status", binary_to_list(StatusText)}]);
update(Pid, Params) ->
  twitter:post(Pid, "https://api.twitter.com/1/statuses/update.json", Params).


%% streaming
user_stream(Pid, Processor) ->
  twitter:oauth_stream(Pid, get, "https://userstream.twitter.com/2/user.json", [], Processor).

sample(Pid, Processor) ->
  twitter:basic_auth_stream(Pid, get, "https://stream.twitter.com/1/statuses/sample.json", [], Processor).

filter(Pid, Track, Processor) ->
  twitter:basic_auth_stream(Pid, get, "https://stream.twitter.com/1/statuses/filter.json", [{"track", Track}], Processor).
