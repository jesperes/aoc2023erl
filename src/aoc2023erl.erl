-module(aoc2023erl).

-export([main/1]).

-include_lib("eunit/include/eunit.hrl").

options() ->
  [ {day,       $d, "day",       integer,    "Day to run"}
  , {benchmark, $b, "benchmark", boolean,    "Benchmark solutions"}
  , {help,      $h, "help",      undefined,  "Show this help"}
  ].

module(Day) ->
  binary_to_atom(iolist_to_binary(io_lib:format("day~2..0w", [Day]))).

main(Args) ->
  io:setopts([{encoding, unicode}]),
  {ok, {Options, _}} = getopt:parse(options(), Args),

  case proplists:get_value(help, Options) of
    true ->
      getopt:usage(options(), escript:script_name());
    undefined ->
      do_main(Options)
  end.

do_main(Options) ->
  Days = case proplists:lookup_all(day, Options) of
           [_|_] = List when is_list(List) ->
             {_, DayNums} = lists:unzip(List),
             DayNums;
           _ ->
             lists:seq(1, 25)
         end,

  {ok, _} = application:ensure_all_started([inets, ssl]),
  % {_, _, Today} = erlang:date(),
  % Days0 = lists:filter(fun(D) -> D =< Today end, Days),

  case proplists:get_value(benchmark, Options) of
    true ->
      io:format("Benchmarking solutions...~n", []);
    _ ->
      io:format("Running solutions...~n", [])
  end,

  Header = ["Day", "Time", "Solution"],
  Rows =
    lists:filtermap(
      fun(D) ->
          Mod = module(D),
          try
            {ok, Input} = input:get(D),
            Expected = input:solution(D),
            {Time, Solution} = run(Mod, Input, Options),

            TimeFmt = lists:flatten(io_lib:format("~w ~tcs", [Time, 16#b5])),

            case Expected == Solution of
              true ->
                {true, [Mod, TimeFmt, Solution]};
              false ->
                {true, [Mod, TimeFmt, "-- incorrect --"]}
            end
          catch
            _:undef ->
              false;
            Class:Reason:St ->
              io:format("~p~n", [St]),
              {true, [Mod, "", {Class, Reason}]}
          end
      end, Days),

  Table = [Header|Rows],
  io:format("~ts~n", [table:format(Table)]).

run(Module, Input, Options) ->
  case proplists:get_value(benchmark, Options, false) of
    true ->
      MaxIter = 1000,
      MaxSecs = 5,
      ItersRemaining = MaxIter,
      TimeRemaining = erlang:convert_time_unit(MaxSecs, second, microsecond),
      run(Module, Input, ItersRemaining, TimeRemaining, 0, 0, undef);
    false ->
      timer:tc(fun() -> Module:solve(Input) end)
  end.

run(_Module, _Input, ItersRemaining, TimeRemaining, AccTime, NumIters, Solution) when ItersRemaining =< 0 orelse TimeRemaining =< 0 ->
  {erlang:trunc(AccTime / NumIters), Solution};
run(Module, Input, ItersRemaining, TimeRemaining, AccTime, NumIters, _) ->
  {Time, Solution} = timer:tc(fun() -> Module:solve(Input) end),
  run(Module, Input, ItersRemaining - 1, TimeRemaining - Time, AccTime + Time, NumIters + 1, Solution).
