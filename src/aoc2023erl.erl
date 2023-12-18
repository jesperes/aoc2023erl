-module(aoc2023erl).

-export([main/1]).

-include_lib("eunit/include/eunit.hrl").

%% It is surprisingly difficult to figure out the OTP version.
otp_version() ->
  {ok, Version} =
    file:read_file(filename:join([code:root_dir(), "releases",
                                  erlang:system_info(otp_release), "OTP_VERSION"])),
  string:trim(binary_to_list(Version)).



module(Day) ->
  binary_to_atom(iolist_to_binary(io_lib:format("day~2..0w", [Day]))).

main(_) ->
  io:setopts([{encoding, unicode}]),
  {ok, _} = application:ensure_all_started([inets, ssl]),
  io:format("Erlang version: ~s~n", [otp_version()]),
  lists:foreach(
    fun(D) ->
        Mod = module(D),
        {Time, Solution} = run(Mod, undef),
        io:format("~-10s~10w μs   ~w~n", [Mod, Time, Solution])
    end, lists:seq(1, 25)).

run(Module, Expected) ->
  MaxIter = 1000,
  MaxSecs = 5,
  run(Module, Expected, MaxIter, erlang:convert_time_unit(MaxSecs, second, microsecond), 0, undef, 0).

run(_Module, _Expected, Iter, _, Time, Val, N) when Iter =< 0 ->
  {erlang:trunc(Time / N), Val};
run(_Module, _Expected, _, Usecs, Time, Val, N) when Usecs =< 0 ->
  {erlang:trunc(Time / N), Val};
run(Module, Expected, MaxIter, MaxUsecs, AccTime, _Val, N) ->
  {Time, Solution} = timer:tc(fun() -> Module:solve() end),
  Descr = io_lib:format("Incorrect solution result for ~s", [Module]),
  ?assertEqual(Expected, Solution, Descr),
  run(Module, Expected, MaxIter - 1, MaxUsecs - Time, AccTime + Time, Solution, N + 1).
