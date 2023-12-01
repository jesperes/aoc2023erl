-module(day06).

-export([ solve/0
        , solve_p2/2
        ]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  solve(input:get(6)).

solve(Bin) ->
  [T, D] = lists:map(fun string:trim/1, binary:split(Bin, <<"\n">>)),
  Times = get_nums(T),
  Distances = get_nums(D),
  {solve_p1(Times, Distances),
   solve_p2(Times, Distances)}.

solve_p1(Times, Distances) ->
  TimeList = lists:map(fun binary_to_integer/1, Times),
  RecordList = lists:map(fun binary_to_integer/1, Distances),
  Races = lists:zip(TimeList, RecordList),
  find_holdtimes(Races).

solve_p2(Times, Distances) ->
  Time = concat_bins_to_int(Times),
  Dist = concat_bins_to_int(Distances),
  find_holdtimes([{Time, Dist}]).

find_holdtimes(Races) ->
  lists:foldl(
    fun({Time, Record}, Acc) ->
        B = -Time,
        C = Record,
        Sq = math:sqrt(B*B - 4 * C),
        X0 = (-B - Sq) / 2,
        X1 = (-B + Sq) / 2,
        Acc * (trunc(X1) - trunc(X0))
    end, 1, Races).

get_nums(Bin) ->
  [_, Bin0] = binary:split(Bin, <<":">>),
  lists:foldl(fun(B, Acc) when byte_size(B) > 0 ->
                  [string:trim(B)|Acc];
                 (_, Acc) -> Acc
              end, [], binary:split(Bin0, <<" ">>, [global])).

concat_bins_to_int(Bins) ->
  binary_to_integer(
    lists:foldl(fun(B, Acc) ->
                    <<B/binary, Acc/binary>>
                end, <<>>, Bins)).

-ifdef(TEST).

solve_test() ->
  ?assertEqual({303600, 23654842}, ?MODULE:solve()).

-endif.
