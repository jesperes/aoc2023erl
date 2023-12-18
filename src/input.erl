-module(input).

-export([get/1, solution/1]).

aoc_session_token() ->
  Home = os:getenv("HOME"),
  {ok, SessionToken} = file:read_file(filename:join(Home, ".adventofcode.session")),
  string:trim(SessionToken).

cache_file(Fmt, Args) ->
  Home = os:getenv("HOME"),
  filename:join([Home, ".cache", "aoc-data", "2023", io_lib:format(Fmt, Args)]).

input_filename(Day) ->
  cache_file("input~w.txt", [Day]).

puzzle_filename(Day) ->
  cache_file("puzzle~w.txt", [Day]).

read_cached_file_or_fetch_from_url(Filename, Url) ->
  case filelib:is_file(Filename) of
    true ->
      file:read_file(Filename);
    false ->
      io:format("~tc Fetching ~s -> ~s~n", [16#1f385, Url, Filename]),
      CookieHeader = io_lib:format("session=~s", [aoc_session_token()]),
      case httpc:request(get, {Url, [{"cookie", CookieHeader}]}, [], [{body_format, binary}]) of
        {ok, {{_, 200, _}, _, Body}} ->
          file:write_file(Filename, Body),
          {ok, Body};
        {ok, {{_, 404, _}, _, _}} ->
          {error, not_found}
      end
  end.

get(Day) ->
  Filename = input_filename(Day),
  Url = io_lib:format("https://adventofcode.com/~w/day/~w/input", [2023, Day]),
  read_cached_file_or_fetch_from_url(Filename, Url).

puzzle_descr(Day) ->
  Filename = puzzle_filename(Day),
  Url = io_lib:format("https://adventofcode.com/~w/day/~w", [2023, Day]),
  read_cached_file_or_fetch_from_url(Filename, Url).

solution(Day) ->
  case puzzle_descr(Day) of
    {ok, Html} ->
      RE = "Your puzzle answer was <code>([^<]+)</code>",
      {match, [[P1], [P2]]} = re:run(Html, RE, [global, {capture, all_but_first, binary}]),
      {convert_to_binary(P1), convert_to_binary(P2)};
    Error ->
      Error
  end.

%% todo
convert_to_binary(P) ->
  binary_to_integer(P).
