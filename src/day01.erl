-module(day01).

-export([solve/1]).

-define(is_digit(X), ((X >= $0) andalso (X =< $9))).

solve(Bin) ->
  Lines = binary:split(Bin, <<"\n">>, [global]),
  {solve0(part1, Lines),
   solve0(part2, Lines)}.

solve0(Part, Lines) ->
  lists:foldl(
    fun(<<>>, Acc) -> Acc;
       (Line, Acc) ->
        Acc + get_first_last(Part, Line, undefined, undefined)
    end, 0, Lines).

get_first_last(_Part, <<>>, First, Last) ->
  list_to_integer([First, Last]);
get_first_last(Part, <<Digit, Rest/binary>>, undefined, undefined) when ?is_digit(Digit) ->
  get_first_last(Part, Rest, Digit, Digit);
get_first_last(Part, <<Digit, Rest/binary>>, First, _) when ?is_digit(Digit) ->
  get_first_last(Part, Rest, First, Digit);
get_first_last(part2, Bin, First, Last) ->
  <<_, Rest/binary>> = Bin,
  Digit = case Bin of
            <<"one", _/binary>> -> $1;
            <<"two", _/binary>> -> $2;
            <<"three", _/binary>> -> $3;
            <<"four", _/binary>> -> $4;
            <<"five", _/binary>> -> $5;
            <<"six", _/binary>> -> $6;
            <<"seven", _/binary>> -> $7;
            <<"eight", _/binary>> -> $8;
            <<"nine", _/binary>> -> $9;
            _ -> undefined
          end,
  case {Digit, First} of
    {undefined, _} -> get_first_last(part2, Rest, First, Last);
    {_, undefined} -> get_first_last(part2, Rest, Digit, Digit);
    _ -> get_first_last(part2, Rest, First, Digit)
  end;
get_first_last(Part, <<_, Rest/binary>>, First, Last) ->
  get_first_last(Part, Rest, First, Last).
