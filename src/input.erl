-module(input).

-export([get/1, do_get/1]).
-compile({parse_transform, ct_expand}).

input_filename(Day) ->
  io_lib:format("priv/input~2..0w.txt", [Day]).

-spec do_get(Day :: integer()) -> binary().
do_get(Day) ->
  Filename = input_filename(Day),
  case file:read_file(Filename) of
    {ok, Binary} -> Binary;
    {error, enoent} ->
      io:format("--- could not find input file ~s~n", [Filename]),
      <<>>
  end.


get(1) -> ct_expand:term(do_get(1));
get(2) -> ct_expand:term(do_get(2));
get(3) -> ct_expand:term(do_get(3));
get(4) -> ct_expand:term(do_get(4));
get(5) -> ct_expand:term(do_get(5));
get(6) -> ct_expand:term(do_get(6));
get(7) -> ct_expand:term(do_get(7)).
