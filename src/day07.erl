-module(day07).

-export([ solve/0
        ]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  solve(input:get(7)).

solve(Bin) ->
  Lines = binary:split(Bin, <<"\n">>, [global]),
  P1 = find_total_winnings(
         Lines,
         fun classify_hand/1,
         [$A, $K, $Q, $J, $T, $9, $8, $7, $6, $5, $4, $3, $2]),

  P2 = find_total_winnings(
         Lines,
         fun classify_hand_with_jokers/1,
         [$A, $K, $Q, $T, $9, $8, $7, $6, $5, $4, $3, $2, $J]),

  {P1, P2}.

find_total_winnings(Lines, ClassifyFun, CardOrder) ->
  SortKeys =
    maps:from_list(
      lists:zip(CardOrder, lists:seq(length(CardOrder), 1, -1))),

  Hands =
    lists:sort(
      lists:foldl(fun(<<>>, Acc) -> Acc;
                     (<<A, B, C, D, E, 32, Bid/binary>>, Acc) ->
                      Hand = [A, B, C, D, E],
                      [{ClassifyFun(Hand),
                        sort_key(Hand, SortKeys),
                        binary_to_integer(Bid)}|Acc]
                  end, [], Lines)),

  Ranks = lists:seq(1, length(Hands)),
  lists:foldl(fun({Rank, {_Type, _Hand, Bid}}, Acc) ->
                  Rank * Bid + Acc
              end, 0, lists:zip(Ranks, Hands)).

sort_key(Hand, SortKeys) ->
  lists:map(fun(Card) ->
                maps:get(Card, SortKeys)
            end, Hand).

classify_hand(Hand) ->
  classify_hand0(lists:sort(Hand)).

%% Five of a kind
classify_hand0([A, A, A, A, A]) -> 6;

%% Four of a kind
classify_hand0([A, A, A, A, _]) -> 5;
classify_hand0([_, A, A, A, A]) -> 5;

%% Full house
classify_hand0([B, B, A, A, A]) -> 4;
classify_hand0([A, A, A, B, B]) -> 4;

%% Three of a kind
classify_hand0([A, A, A, _, _]) -> 3;
classify_hand0([_, A, A, A, _]) -> 3;
classify_hand0([_, _, A, A, A]) -> 3;

%% Two pair
classify_hand0([A, A, B, B, _]) -> 2;
classify_hand0([A, A, _, B, B]) -> 2;
classify_hand0([_, A, A, B, B]) -> 2;

%% One pair
classify_hand0([A, A, _, _, _]) -> 1;
classify_hand0([_, A, A, _, _]) -> 1;
classify_hand0([_, _, A, A, _]) -> 1;
classify_hand0([_, _, _, A, A]) -> 1;

%% High cards
classify_hand0(_) -> 0.

%% Classify hand with joker semantics by generating all possible
%% combination of joker values, classifying them and picking the best
%% one.
classify_hand_with_jokers([A, B, C, D, E]) ->
  NonJokers = [$A, $K, $Q, $T, $9, $8, $7, $6, $5, $4, $3, $2],
  ExpandedHands =
    [[A0, B0, C0, D0, E0] ||
       A0 <- ?IF(A == $J, NonJokers, [A]),
       B0 <- ?IF(B == $J, NonJokers, [B]),
       C0 <- ?IF(C == $J, NonJokers, [C]),
       D0 <- ?IF(D == $J, NonJokers, [D]),
       E0 <- ?IF(E == $J, NonJokers, [E])],
  lists:foldl(fun(JokerHand, Best) ->
                  Type = classify_hand(JokerHand),
                  ?IF(Type > Best, Type, Best)
              end, 0, ExpandedHands).

-ifdef(TEST).

solve_test() ->
  ?assertEqual({248836197,251195607}, ?MODULE:solve()).

-endif.
