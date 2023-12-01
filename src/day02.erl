-module(day02).

-export([solve/0]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  solve(input:get(2)).

solve(Bin) ->
  lists:foldl(
    fun(<<>>, Acc) -> Acc;
       (<<"Game ", Line/binary>>, {P1, P2}) ->
        [Game|Draws] = string:tokens(binary_to_list(Line), ":,;"),
        {GameId, {R, G, B}} =
          lists:foldl(
            fun(Draw, {GameIn, {R, G, B} = RGB}) ->
                [NumStr, [Color|_]] = string:tokens(Draw, " "),
                N = list_to_integer(NumStr),
                {case {GameIn, Color} of
                   {GI, $r} when GI >= 0 andalso N =< 12 -> GameIn;
                   {GI, $g} when GI >= 0 andalso N =< 13 -> GameIn;
                   {GI, $b} when GI >= 0 andalso N =< 14 -> GameIn;
                   _ -> 0
                 end,
                 case Color of
                   $r when N > R -> {N, G, B};
                   $g when N > G -> {R, N, B};
                   $b when N > B -> {R, G, N};
                   _ -> RGB
                 end}
            end, {list_to_integer(Game), {0, 0, 0}}, Draws),
        {GameId + P1, R * G * B + P2}
    end, {0, 0}, binary:split(Bin, <<"\n">>, [global])).

-ifdef(TEST).

solve_test() ->
  ?assertEqual({2061,72596}, ?MODULE:solve()).

-endif.
