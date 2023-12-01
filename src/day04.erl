-module(day04).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  solve(input:get(4)).

solve(Bin) ->
  Lines = binary:split(Bin, <<"\n">>, [global]),

  {NumCards, Sum, Map} =
    lists:foldl(
      fun(<<>>, Acc) -> Acc;
         (<<"Card ", Line/binary>>, {N, Sum0, Acc}) ->
          [CardBin, NumbersBin] = binary:split(Line, <<":">>),
          [WinningNumbers, YourNumbers] = binary:split(NumbersBin, <<"|">>),
          WN = split_nums(WinningNumbers),
          YN = split_nums(YourNumbers),
          NumMatching = num_intersecting(WN, YN),
          Card = binary_to_integer(string:trim(CardBin)),

          {N + 1, Sum0 + (1 bsl (NumMatching - 1)),
           lists:foldl(
             fun(I, Map0) ->
                 CardVal = maps:get(Card, Map0, 0) + 1,
                 sum_map(I, CardVal, Map0)
             end, Acc, lists:seq(Card + 1, Card + NumMatching))}
      end, {0, 0, #{}}, Lines),

  {Sum, lists:sum(maps:values(Map)) + NumCards}.

sum_map(Key, Val, Map) ->
  maps:update_with(Key, fun(Old) -> Val + Old end, Val, Map).

num_intersecting(List1, List2) ->
  Set1 = sets:from_list(List1),
  Set2 = sets:from_list(List2),
  length(sets:to_list(sets:intersection(Set1, Set2))).

split_nums(Bin) ->
  lists:filter(fun(X) ->
                   byte_size(X) > 0
               end,
               binary:split(Bin, <<" ">>, [global])).


-ifdef(TEST).

solve_test() ->
  ?assertEqual({19855, 10378710}, ?MODULE:solve()).

-endif.
