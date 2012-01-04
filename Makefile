.SUFFIXES:
.SUFFIXES: .erl .beam

ERLC = erlc
ERLFLAGS = 

.erl.beam:
	$(ERLC) $(ERLFLAGS) $*.erl


TARGETS = oauth.beam mochijson2.beam \
	streaming.beam oauth_stream.beam basic_auth_stream.beam \
	twitter.beam twitter_api.beam \
    sample.beam

all: $(TARGETS)

clean:
	rm -f *.beam *~

mochijson2:  mochijson2.beam

oauth: oauth.beam # lists proplists base64 crypto public_key string httpc calendar file re http_uri

streaming: streaming.beam

oauth_stream: oauth_stream.beam \
	oauth streaming # base64 calendar crypto file httpc http_uri io lists proplists public_key re string

basic_auth_stream: basic_auth_stream.beam \
	oauth streaming # base64 httpc

twitter: twitter.beam twitter.hrl \
	oauth_stream # base64 httpc io

twitter_api: twitter_api.beam \
	twitter

sample: sample.beam \
    twitter twitter_api mochijson2

