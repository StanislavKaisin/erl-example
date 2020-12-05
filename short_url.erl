-module(short_url).

-export([start/0, stop/0, restart/0, short/1, long/1]).
% -export([rand_char/0, rand_string/1]).

% API methods
start()->
  io:format("start called~n"),
  Pid=spawn(fun()->loop(dict:new()) end),
  % register this process
  register(short_url_server, Pid),
  Pid.

stop()->
  io:format("stop called~n"),
  short_url_server ! stop,
  ok.

restart()->
  stop(),
  start().

short(LongUrl)->
  io:format("short for ~p called~n", [LongUrl]),
  Uniq=make_ref(),
  short_url_server ! {short, LongUrl, self(), Uniq},
  receive
    {Uniq, Ans}->Ans
  end.

long(ShortUrl)->
  io:format("long for ~p called~n", [ShortUrl]),
  Uniq=make_ref(),
  short_url_server ! {long, ShortUrl, self(), Uniq},
  receive
    {Uniq, Ans}->Ans
  end.

% main loop
loop(State)-> 
  io:format("~p wait for mesages~n", [self()]),
  receive
    {short, LongUrl, From, Uniq}-> 
      % io:format("short mesage received~n"),
      {Res, NewState} = case dict:is_key(LongUrl, State) of
        true->{dict:fetch(LongUrl, State), State};
        false->ShortUrl="http://short.by/"++rand_string(7),
               {ShortUrl, dict:store(LongUrl, ShortUrl, State)}
        end,
      io:format("Res= ~p~n", [Res]),
      From ! {Uniq, Res},
      loop(NewState);
    {long, ShortUrl, From, Uniq}-> %something
      % io:format("long mesage received~n"),
      FDict= dict:filter(fun(_Key, Value)-> Value =:= ShortUrl end, State),
      FList=dict:to_list(FDict),
      Res = case FList of 
        []->"";
        [{Ans, _}]-> Ans;
        Any->io:format("Any= ~p~n", [Any]), ""
      end,
      io:format("Res= ~p~n", [Res]),
      From ! {Uniq, Res},
      loop(State);
    stop-> ok;
    Msg-> io:format("error: unknown message ~p~n", [Msg]),
      loop(State)
end.

% internal methods
rand_string(Length)-> 
  L=lists:seq(1, Length),
  lists:flatten([rand_char() || _Index <-L]).


rand_char()->
  Chars="qwertyuiopasdfghjklzxcvbnm",
  Index=random:uniform(length(Chars)),
  lists:nth(Index, Chars).



% fun short(other)->
%   other;
% fun short(Num)->
%   Num;
% fun short({user, Name})->
%   Name;
% fun short(some)->
%   some.

% fun short(Num)->
%   case Num of 
%     other->other;
%     some->some;
%     22->22;
%     _-> error
% end.

% c(short_url). 
% short_url:start().
% short_url:stop().
% short_url:short("http://google.com").
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
% L=[{"aa", "bb"}].