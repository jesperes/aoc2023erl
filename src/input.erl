-module(input).

-export([get/1, solution/1]).

aoc_session_token() ->
  Home = os:getenv("HOME"),
  {ok, SessionToken} = file:read_file(filename:join(Home, ".adventofcode.session")),
  string:trim(SessionToken).

input_filename(Day) ->
  Home = os:getenv("HOME"),
  filename:join([Home, ".cache", "aoc-data", "2023",
                 io_lib:format("input~w.txt", [Day])]).

puzzle_filename(Day) ->
  Home = os:getenv("HOME"),
  filename:join([Home, ".cache", "aoc-data", "2023",
                 io_lib:format("puzzle~w.txt", [Day])]).

read_cached_file_or_fetch_from_url(Filename, Url) ->
  case filelib:is_file(Filename) of
    true ->
      file:read_file(Filename);
    false ->
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
      {P1, P2};
    Error ->
      Error
  end.
