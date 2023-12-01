-module(day05).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  solve(input:get(5)).

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
  ?debugVal({Start, Len}),
  ?debugVal({XStart, XLen}),
  if
    %% Case 1: seed range is smaller and disjoint
    Start + Len =< XStart ->
      ?debugVal(case_1),
      transform2(SeedRangesRest, TransformRanges, [SeedRange|Acc]);

    %% Case 2: x-range is smaller and disjoint
    XStart + Len =< Start ->
      ?debugVal(case_2),
      transform2(SeedRanges, TransformRangesRest, Acc);

    %% Case 3: seed range is smaller, and partially overlaps with the
    %% lower part of the x-range.
    Start < XStart andalso Start + Len =< XStart + XLen ->
      ?debugVal(case_3),
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

-ifdef(TEST).

transform2_test_() ->
  [ {"Test case 1", ?_assertEqual([{1, 5}], transform2([{1, 5}], [{1000, 7, 5}]))}
  , {"Test case 2", ?_assertEqual([{7, 5}], transform2([{7, 5}], [{1000, 1, 5}]))}
  , {"Test case 3", ?_assertEqual([{1, 2}, {1002, 3}], transform2([{1, 5}], [{1000, 3, 5}]))}
  , {"Test case 4", ?_assertEqual([{1001, 3}], transform2([{3, 3}], [{1000, 2, 5}]))}
  ].
  %% Ranges = [{45, 77, 23},
  %%           {81, 45, 19},
  %%           {68, 64, 13}],
  %% [{"Case 1", ?_assertEqual([], transform2([{82, 1}], Ranges))}].

%% transform2_test_() ->
%%   Range = {100, 3, 3},
%%   [ {"Case 1", ?_assertEqual([{0, 2}], transform2_one_range({0, 2}, Range))}
%%   , {"Case 2", ?_assertEqual([{2, 1}, {100, 1}], transform2_one_range({2, 2}, Range))}
%%   , {"Case 3", ?_assertEqual([{101, 1}], transform2_one_range({4, 1}, Range))}
%%   , {"Case 4", ?_assertEqual([{102, 1}, {6, 1}], transform2_one_range({5, 2}, Range))}
%%   , {"Case 5", ?_assertEqual([{7, 2}], transform2_one_range({7, 2}, Range))}
%%   , {"Case 6", ?_assertEqual([{2, 1}, {100, 3}, {6, 1}], transform2_one_range({2, 5}, Range))}
%%   , {"Misc 7", ?_assertEqual([{2, 1}, {100, 3}], transform2_one_range({2, 4}, Range))}
%%   , {"Misc 8", ?_assertEqual([{6, 3}], transform2_one_range({6, 3}, Range))}
%%   , {"Misc 9", ?_assertEqual([{101, 2}], transform2_one_range({4, 2}, Range))}
%%   , {"Misc 10", ?_assertEqual([{0, 3}, {100, 3}], transform2_one_range({0, 6}, Range))}
%%   ].

%% transform2_errors_test_() ->
%%   Range = {100, 3, 3},
%%   [ {"Error 1", ?_assertEqual([{56, 4}, {97, 2}], transform2_one_range({93, 6}, {56, 93, 4}))}
%%   , {"Error 2", ?_assertEqual([{100, 3}], transform2_one_range({3, 3}, Range))}
%%   ].

%% example() ->
%%   <<"seeds: 79 14 55 13\n",
%%     "\n",
%%     "seed-to-soil map:\n",
%%     "50 98 2\n",
%%     "52 50 48\n",
%%     "\n",
%%     "soil-to-fertilizer map:\n",
%%     "0 15 37\n",
%%     "37 52 2\n",
%%     "39 0 15\n",
%%     "\n",
%%     "fertilizer-to-water map:\n",
%%     "49 53 8\n",
%%     "0 11 42\n",
%%     "42 0 7\n",
%%     "57 7 4\n",
%%     "\n",
%%     "water-to-light map:\n",
%%     "88 18 7\n",
%%     "18 25 70\n",
%%     "\n",
%%     "light-to-temperature map:\n",
%%     "45 77 23\n",
%%     "81 45 19\n",
%%     "68 64 13\n",
%%     "\n",
%%     "temperature-to-humidity map:\n",
%%     "0 69 1\n",
%%     "1 0 69\n",
%%     "\n",
%%     "humidity-to-location map:\n",
%%     "60 56 37\n",
%%     "56 93 4\n">>.

%% example_test() ->
%%    ?assertEqual({35, 46}, solve(example())).

%% transform_test() ->
%%   Ranges = [{50, 98, 2}, {52, 50, 48}],
%%   ?assertEqual(81, transform(79, Ranges)),
%%   ?assertEqual(14, transform(14, Ranges)),
%%   ?assertEqual(57, transform(55, Ranges)),
%%   ?assertEqual(13, transform(13, Ranges)).

%%solve_test() ->
%%  ?assertEqual({282277027, 11554135}, ?MODULE:solve()).

-endif.
