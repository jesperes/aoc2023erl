-module(day03).

-export([solve/1]).

-define(is_digit(D), (D >= $0 andalso D =< $9)).

solve(Bin) ->
  {W, _} = binary:match(Bin, <<"\n">>),
  WNL = W + 1,
  Map = lists:foldl(
          fun(PosLen, Acc) ->
              find_adj_symbols(PosLen, WNL, Bin, Acc)
          end, #{}, find_numbers(Bin)),

  %% `Map' is a map from symbols (their index) to the set of numbers
  %% they are adjacent to.

  {_, L0} = lists:unzip(lists:flatten(lists:map(fun sets:to_list/1, maps:values(Map)))),
  P1 = lists:sum(L0),

  P2 = maps:fold(
         fun({_, $*}, Numbers, Sum) ->
             case sets:size(Numbers) of
               N when N == 2 ->
                 [{_, Gear1}, {_, Gear2}] = sets:to_list(Numbers),
                 Sum + Gear1 * Gear2;
               _ ->
                 Sum
             end;
            (_, _, Sum) ->
             Sum
         end, 0, Map),

  {P1, P2}.

%% Find the location of all the numbers. This is roughly equivalent to
%% re:run(Bin, "(\\d+)", [global]), but faster.
find_numbers(Bin) ->
  find_numbers(Bin, 0, [], false, 0, 0, 0).

find_numbers(<<>>, _, Numbers, _, _, _, _) ->
  lists:reverse(Numbers);
find_numbers(<<D, Rest/binary>>, Index, Numbers, true, Curr, NumLen, NumStart) when ?is_digit(D) ->
  find_numbers(Rest, Index + 1, Numbers,
               true, Curr * 10 + (D - $0), NumLen + 1, NumStart);
find_numbers(<<D, Rest/binary>>, Index, Numbers, false, _Curr, _NumLen, _NumStart) when ?is_digit(D) ->
  find_numbers(Rest, Index + 1, Numbers, true, D - $0, 1, Index);
find_numbers(<<_, Rest/binary>>, Index, Numbers, true, _Curr, NumLen, NumStart) ->
  find_numbers(Rest, Index + 1, [{NumStart, NumLen}|Numbers], false, 0, 0, 0);
find_numbers(<<_, Rest/binary>>, Index, Numbers, false, CurrNum, NumLen, NumStart) ->
  find_numbers(Rest, Index + 1, Numbers, false, CurrNum, NumLen, NumStart).

find_adj_symbols({Start, Len} = PosLen, W, Bin, Map) ->
  N = binary_to_integer(binary:part(Bin, PosLen)),
  lists:foldl(
    fun(Idx, Map0) ->
        Adjacents = [Idx - 1,
                     Idx + 1,
                     Idx - W - 1,
                     Idx - W,
                     Idx - W + 1,
                     Idx + W - 1,
                     Idx + W,
                     Idx + W + 1],

        lists:foldl(
          fun(AdjIdx, Map1) ->
              if AdjIdx >= 0 andalso AdjIdx < byte_size(Bin) ->
                  C = binary:at(Bin, AdjIdx),
                  if C >= $0 andalso C =< $9 -> Map1;
                     C == $. -> Map1;
                     C == $\n -> Map1;
                     true ->
                      maps:update_with(
                        {AdjIdx, C},
                        fun(Old) ->
                            sets:add_element({Start, N}, Old)
                        end,
                        sets:from_list([{Start, N}]), Map1)
                  end;
                 true ->
                  %% outside the grid
                  Map1
              end
          end, Map0, Adjacents)
    end, Map, lists:seq(Start, Start + Len - 1)).
