-module(aoc2023erl).

-export([main/1]).

-include_lib("eunit/include/eunit.hrl").


module(Day) ->
  binary_to_atom(iolist_to_binary(io_lib:format("day~2..0w", [Day]))).
main(_) ->
  io:setopts([{encoding, unicode}]),
  {ok, _} = application:ensure_all_started([inets, ssl]),
  {_, _, Today} = erlang:date(),
  lists:foreach(
    fun(D) when D > Today ->
        %% Not released yet, ignore
        ok;
       (D) ->
        Mod = module(D),
        try
          {ok, Input} = input:get(D),
          Expected = input:solution(D),
          {Time, Solution} = run(Mod, Input),

          case Expected == Solution of
            true ->
              io:format("~-10s~10w μs   ~tc ~0p~n", [Mod, Time, 16#2713, Solution]);
            false ->
              io:format("~-10s~10w μs   ~tc ~0p (expected ~0p)~n", [Mod, Time, 16#2717, Solution, Expected])
          end
        catch
          _:undef ->
            io:format("~-10s not implemented~n", [Mod]);
          _Class:Reason ->
            io:format("~-10s                ~tc EXCEPTION: ~0p~n", [Mod, 16#2717, Reason])
        end
    end, lists:seq(1, 25)).

run(Module, Input) ->
  MaxIter = 1000,
  MaxSecs = 5,
  ItersRemaining = MaxIter,
  TimeRemaining = erlang:convert_time_unit(MaxSecs, second, microsecond),
  run(Module, Input, ItersRemaining, TimeRemaining, 0, 0, undef).

run(_Module, _Input, ItersRemaining, TimeRemaining, AccTime, NumIters, Solution) when ItersRemaining =< 0 orelse TimeRemaining =< 0 ->
  {erlang:trunc(AccTime / NumIters), Solution};
run(Module, Input, ItersRemaining, TimeRemaining, AccTime, NumIters, _) ->
  {Time, Solution} = timer:tc(fun() -> Module:solve(Input) end),
  run(Module, Input, ItersRemaining - 1, TimeRemaining - Time, AccTime + Time, NumIters + 1, Solution).
