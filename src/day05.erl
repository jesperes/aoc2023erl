-module(day05).

-export([solve/1]).

-include_lib("eunit/include/eunit.hrl").

solve(Bin) ->
  [<<"seeds: ", Seeds/binary>>|Maps] = binary:split(Bin, <<"\n\n">>, [global]),
  SeedList =
    lists:map(fun binary_to_integer/1, binary:split(Seeds, <<" ">>, [global])),
  Almanac =
    lists:foldl(
      fun(MapBin, Acc) ->
          [Name|MapLines] = binary:split(MapBin, <<"\n">>, [global]),
          [Source, _, Dest|_] = binary:split(Name, <<"-">>, [global]),
          [Dest0|_] = binary:split(Dest, <<" map:">>),
          Ranges =
            lists:map(
              fun(RangeBin) ->
                  [DestRangeStart, SourceRangeStart, RangeLen] = binary:split(RangeBin, <<" ">>, [global]),
                  {binary_to_integer(DestRangeStart),
                   binary_to_integer(SourceRangeStart),
                   binary_to_integer(RangeLen)}
              end, lists:filter(fun(B) -> byte_size(B) > 0 end, MapLines)),
          maps:put(Source, {Dest0, Ranges}, Acc)
      end, #{}, Maps),

  %% Part 1
  P1 = solve_part(SeedList, Almanac, fun transform1/2),
  P2 = solve_part(seed_ranges(SeedList), Almanac, fun transform2/2),
  {P1, P2}.

solve_part(Seeds, Almanac, TransformFun) ->
  case
    lists:sort(
      lists:flatten(
        lists:map(
          fun(Seed) ->
              convert_seed(Seed, <<"seed">>, Almanac, TransformFun)
          end,
          Seeds)))
  of
    [{Solution, _}|_] -> Solution;
    [Solution|_] when is_integer(Solution) -> Solution;
    Other -> {unknown_solution, Other}
  end.

convert_seed(Seed, <<"location">>, _, _) ->
  Seed;
convert_seed(SourceSeed, SourceCategory, Almanac, TransformFun) ->
  {TargetCategory, TransformRanges} = maps:get(SourceCategory, Almanac),
  TargetSeed = TransformFun(SourceSeed, TransformRanges),
  convert_seed(TargetSeed, TargetCategory, Almanac, TransformFun).

%% Part 1: seed is a single number
transform1(Num, []) -> Num;
transform1(Num, [{DestRangeStart, SourceRangeStart, RangeLen}|Ranges]) ->
  if Num >= SourceRangeStart andalso Num < SourceRangeStart + RangeLen ->
      Num - SourceRangeStart + DestRangeStart;
     true ->
      transform1(Num, Ranges)
  end.

%% Part 2: seed is a list of ranges
seed_ranges(List) -> seed_ranges(List, []).
seed_ranges([], Acc) -> Acc;
seed_ranges([A, B|Rest], Acc) -> seed_ranges(Rest, [[{A, B}]|Acc]).

transform2(SeedRanges, TransformRanges) ->
  transform2(lists:sort(SeedRanges), lists:sort(TransformRanges), []).

%% "x range" is short for "transforming range"
transform2([], _, Acc) ->
  %% No seed ranges left, we are done.
  Acc;
transform2(SeedRanges, [], Acc) ->
  %% No transforming ranges left, use remaining seed ranges unmapped
  SeedRanges ++ Acc;
transform2([{Start, Len} = SeedRange|SeedRangesRest] = SeedRanges,
           [{XDest, XStart, XLen} = XRange|TransformRangesRest] = TransformRanges, Acc) ->
  %%?debugVal({Start, Len}),
  %%?debugVal({XStart, XLen}),
  if
    %% Case 1: seed range is smaller and disjoint
    Start + Len =< XStart ->
      ?debugVal(case_1),
      transform2(SeedRangesRest, TransformRanges, [SeedRange|Acc]);

    %% Case 2: x-range is smaller and disjoint
    XStart + Len =< Start ->
  %%    ?debugVal(case_2),
      transform2(SeedRanges, TransformRangesRest, Acc);

    %% Case 3: seed range is smaller, and partially overlaps with the
    %% lower part of the x-range.
    Start < XStart andalso Start + Len =< XStart + XLen ->
%%      ?debugVal(case_3),
      SplitLen = XStart - Start,
      Left = {Start, SplitLen},
      Right = {XDest + SplitLen, Len - SplitLen},
      transform2(SeedRangesRest, TransformRanges, [Left, Right|Acc]);

    %% Case 4: seed range is fully enclosed in the x-range, so
    %% the entire seed range gets transformed
    Start >= XStart andalso Start + Len =< XStart + XLen ->
      transform2(SeedRangesRest, TransformRanges, [{XDest + (Start - XStart), Len}|Acc]);

    %% Case 5: x-range is smaller, and partially overlaps with the lower
    %% part of the seed-range

    true ->
      throw({no_rule, SeedRange, XRange})
  end.
