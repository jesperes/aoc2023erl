-module(day08).

-export([solve/1, solve/0]).

-include_lib("eunit/include/eunit.hrl").

solve() ->
  {ok, Bin} = input:get(8),
  solve(Bin).

solve(Bin) ->
  [Instrs, Rest] = binary:split(Bin, <<"\n">>),
  Rules =
    maps:from_list(
      lists:foldl(fun(<<>>, Acc) -> Acc;
                     (<<A1, A2, A3, _:32, B1, B2, B3, _:16, C1, C2, C3, _/binary>>, Acc) ->
                      A = <<A1, A2, A3>>,
                      B = <<B1, B2, B3>>,
                      C = <<C1, C2, C3>>,
                      [{A, {B, C}}|Acc]
                  end, [], binary:split(Rest, <<"\n">>, [global]))),

  {solve_p1(Instrs, Rules),
   solve_p2(Instrs, Rules)}.

solve_p1(Instrs, Rules) ->
  solve_p1(<<"AAA">>, Instrs, Instrs, Rules, 0).

solve_p1(<<"ZZZ">>, _, _, _, Steps) ->
  Steps;
solve_p1(Current, <<>>, Instrs, Rules, Steps) ->
  solve_p1(Current, Instrs, Instrs, Rules, Steps);
solve_p1(Current, <<$L, Rest/binary>>, Instrs, Rules, Steps) ->
  {L, _} = maps:get(Current, Rules),
  solve_p1(L, Rest, Instrs, Rules, Steps + 1);
solve_p1(Current, <<$R, Rest/binary>>, Instrs, Rules, Steps) ->
  {_, R} = maps:get(Current, Rules),
  solve_p1(R, Rest, Instrs, Rules, Steps + 1).

%% Part 2
solve_p2(Instrs, Rules) ->
  Starts =
    lists:filter(
      fun(<<_, _, $A>>) -> true;
         (_) -> false
      end, maps:keys(Rules)),

  Cycles =
    lists:map(fun(Start) ->
                  solve_one_p2(Start, Instrs, Rules)
              end, Starts),

  lists:foldl(fun lcm/2, 1, Cycles).

solve_one_p2(Start, Instrs, Rules) ->
  solve_one_p2(Start, Instrs, Instrs, Rules, 0).

solve_one_p2(<<_, _, $Z>>, _, _, _, Steps) ->
  Steps;
solve_one_p2(Current, <<>>, Instrs, Rules, Steps) ->
  solve_one_p2(Current, Instrs, Instrs, Rules, Steps);
solve_one_p2(Current, <<$L, Rest/binary>>, Instrs, Rules, Steps) ->
  {L, _} = maps:get(Current, Rules),
  solve_one_p2(L, Rest, Instrs, Rules, Steps + 1);
solve_one_p2(Current, <<$R, Rest/binary>>, Instrs, Rules, Steps) ->
  {_, R} = maps:get(Current, Rules),
  solve_one_p2(R, Rest, Instrs, Rules, Steps + 1).

gcd(A,B) when A == 0; B == 0 -> 0;
gcd(A,B) when A == B -> A;
gcd(A,B) when A > B -> gcd(A-B, B);
gcd(A,B) -> gcd(A, B-A).

lcm(A,B) -> (A*B) div gcd(A, B).
