-module(table).

-export([ format/2
        , format/1
        ]).

-include_lib("eunit/include/eunit.hrl").

-define(CORNER_UPPER_LEFT, $+).
-define(CORNER_UPPER_RIGHT, $+).
-define(CORNER_LOWER_LEFT, $+).
-define(CORNER_LOWER_RIGHT, $+).

-define(HORIZONTAL, $-).
-define(VERTICAL, $|).

-define(VERTICAL_RIGHT, $+).
-define(VERTICAL_LEFT, $+).
-define(HORIZONTAL_UP, $+).
-define(HORIZONTAL_DOWN, $+).

-define(NL, $\n).

-define(HORIZ_SPACING, 1).

format(Rows) ->
  format(Rows, []).

format(Rows, _Options) ->
  {ColWidths, [Header|Rows0]} = normalize_rows(Rows),

  [ top_border(ColWidths)
  , header(Header, ColWidths)
  , header_separator(ColWidths)
  , body(Rows0, ColWidths)
  , bottom_border(ColWidths)
  ].

top_border(ColWidths) ->
  HorizPad = lists:duplicate(?HORIZ_SPACING, ?HORIZONTAL),
  [?CORNER_UPPER_LEFT] ++
    HorizPad ++

    lists:join(
      [HorizPad, ?HORIZONTAL_DOWN, HorizPad],
      lists:map(fun(Width) ->
                    lists:duplicate(Width, ?HORIZONTAL)
                end, ColWidths)) ++

    HorizPad ++
    [?CORNER_UPPER_RIGHT] ++
    [?NL].


header(Header, ColWidths) ->
  HorizPad = lists:duplicate(?HORIZ_SPACING, 32),
  [?VERTICAL] ++
    HorizPad ++
    lists:join(
      [HorizPad, ?VERTICAL, HorizPad],
      lists:map(fun({Text, Width}) ->
                    pad_to_length(Text, 32, Width)
                end, lists:zip(Header, ColWidths))) ++
    HorizPad ++
    [?VERTICAL] ++ [?NL].

header_separator(ColWidths) ->
  HorizPad = lists:duplicate(?HORIZ_SPACING, ?HORIZONTAL),
  [?VERTICAL_RIGHT] ++
    HorizPad ++

    lists:join(
      [HorizPad, ?HORIZONTAL_DOWN, HorizPad],
      lists:map(fun(Width) ->
                    lists:duplicate(Width, ?HORIZONTAL)
                end, ColWidths)) ++

    HorizPad ++
    [?VERTICAL_LEFT] ++
    [?NL].

body(Rows, ColWidths) ->
  lists:map(fun(Row) ->
                body_row(Row, ColWidths)
            end, Rows).

body_row(Row, ColWidths) ->
  HorizPad = lists:duplicate(?HORIZ_SPACING, 32),
  [?VERTICAL] ++
    HorizPad ++
    lists:join(
      [HorizPad, ?VERTICAL, HorizPad],
      lists:map(
        fun({Text, Width}) ->
            pad_to_length(Text, 32, Width)
        end, lists:zip(Row, ColWidths))) ++
    HorizPad ++
    [?VERTICAL] ++ [?NL].

bottom_border(ColWidths) ->
  HorizPad = lists:duplicate(?HORIZ_SPACING, ?HORIZONTAL),
  [?CORNER_LOWER_LEFT] ++
    HorizPad ++

    lists:join(
      [HorizPad, ?HORIZONTAL_DOWN, HorizPad],
      lists:map(fun(Width) ->
                    lists:duplicate(Width, ?HORIZONTAL)
                end, ColWidths)) ++

    HorizPad ++
    [?CORNER_LOWER_RIGHT] ++
    [?NL].

%% Internals

pad_to_length(L, _Pad, N) when length(L) == N ->
  L;
pad_to_length(L, Pad, N) when length(L) < N ->
  L ++ lists:duplicate(N - length(L), Pad).


format_cell(Text) when is_list(Text) ->
  Text;
format_cell(Bin) when is_binary(Bin) ->
  binary_to_list(Bin);
format_cell(Other) ->
  lists:flatten(io_lib:format("~0p", [Other])).

normalize_rows(Rows) ->
  NumCols = lists:max(lists:map(fun erlang:length/1, Rows)),

  PaddedRows =
    lists:map(
      fun(Row) ->
          Row0 = lists:map(fun format_cell/1, Row),
          pad_to_length(Row0, "", NumCols)
      end, Rows),

  {lists:map(
     fun(ColNum) ->
         lists:max(
           lists:map(fun(Row) ->
                         length(lists:nth(ColNum, Row))
                     end, PaddedRows))
     end, lists:seq(1, NumCols)),
   PaddedRows}.


-ifdef(TEST).

pad_to_length_test_() ->
  ?_assertEqual([1, 2, 3, pad, pad, pad],
                pad_to_length([1, 2, 3], pad, 6)).

-endif.
