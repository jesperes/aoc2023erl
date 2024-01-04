-module(day09).

-export([solve/1]).

solve(Bin) ->
  Input =
    lists:filtermap(fun(<<>>) -> false;
                       (Line) ->
                        {true, lists:map(fun binary_to_integer/1, binary:split(Line, <<" ">>, [global]))}
                    end, binary:split(Bin, <<"\n">>, [global])),

  {do_solve(Input, fun predict_next/1),
   do_solve(Input, fun predict_prev/1)}.


do_solve(Input, NextFun) ->
  lists:sum(lists:map(NextFun, Input)).

predict_next(Seq) ->
  case lists:all(fun is_zero/1, Seq) of
    true -> 0;
    false -> lists:last(Seq) + predict_next(next_seq(Seq))
  end.

predict_prev([Head|_] = Seq) ->
  case lists:all(fun is_zero/1, Seq) of
    true -> 0;
    false -> Head - predict_prev(next_seq(Seq))
  end.

is_zero(0) -> true;
is_zero(_) -> false.

next_seq(Seq) ->
   next_seq(Seq, []).

next_seq([A, B|Rest], Acc) ->
  next_seq([B|Rest], [B - A|Acc]);
next_seq(_, Acc) ->
  lists:reverse(Acc).
