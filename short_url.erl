-module(short_url).

-behavior(gen_server). %implements IGenServer

-export([start/0, stop/0, restart/0, short/1, long/1]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


% -export([rand_char/0, rand_string/1]).

% API methods
start()->
  gen_server:start_link({local, shurl}, short_url, [], []).


stop()->
  io:format("normal stop called~n"),
  gen_server:cast(shurl, stop).

restart()->
  stop(),
  start().

short(LongUrl)->
  gen_server:call(shurl, {short, LongUrl}).

long(ShortUrl)->
  gen_server:call(shurl, {long, ShortUrl}).

% gen_server API
init([])->
  io:format("start server~n"),
  {ok, {dict:new(), dict:new()}}.

% S2L - Short to Long
% L2 - Long to Short
handle_call({short, LongUrl}, _From, {S2L, L2S}=State)->
  {Res, NewState} = case dict:is_key(LongUrl, L2S) of
    true-> 
      {dict: fetch(LongUrl, L2S), {S2L, L2S}};
    false-> 
      ShortUrl="http://sh.by/" ++ rand_string(7),
      NewS2L=dict:store(ShortUrl, LongUrl, S2L),

      NewL2S=dict:store(LongUrl, ShortUrl, L2S),

      {ShortUrl, {NewS2L, NewL2S}}
  end,
  {reply, Res, NewState};

  handle_call({long, ShortUrl}, _From, {S2L, _}=State)->
    Res= case dict:is_key(ShortUrl, S2L) of
      true->dict:fetch(ShortUrl, S2L);
      false -> ""
    end,
    {reply, Res, State};

handle_call(Msg, From, State)->
  error_logger:error_msg("unknown msg ~p from ~p ~n", [Msg, From]),
  {noreply, State}.


handle_cast(stop, State)->
  io:format("normal stop ~n"),
  {stop, normal, State};

handle_cast(Msg, State) ->
  error_logger: error_msg("unknown msg ~p ~n", [Msg]),
  {noreply, State}.


handle_info(Msg, State)->
  error_logger: error_msg("unknown msg ~p ~n", [Msg]),
  {noreply, State}.


terminate(_Reason, _State)->
  ok.


code_change(_OldVersion, State, _Extra)->
  {ok, State}.



% internal methods
rand_string(Length)-> 
  L=lists:seq(1, Length),
  lists:flatten([rand_char() || _Index <-L]).


rand_char()->
  Chars="qwertyuiopasdfghjklzxcvbnm",
  Index=random:uniform(length(Chars)),
  lists:nth(Index, Chars).


% c(short_url). 
% short_url:start().
% short_url:stop().
% short_url:short("http://google.com").
% short_url:long("http://sh.by/slnfohb").
% short_url:short("http://mail.com").
% short_url:long("http://dd").
% short_url:rand_char().
% short_url:rand_string(5).

% short_url:short("http://google.com").
% short_url:short("http://mail.com").
% short_url:short("http://tyt.by").

% short_url:long("http://short.by/slnfohb").
% short_url:long("http://short.by/kdhrylt").
% short_url:long("http://short.by/gysaqgd").



% short_url:restart().
