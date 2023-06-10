-module(day_22_9).

-export([solve/1]).

parse(Data) ->
  Lines = binary:split(Data, <<"\n">>, [global, trim]),
  Instructions = lists:map(fun parse_instruction/1, Lines),
  Instructions.

parse_instruction(Line) ->
  [A, B] = binary:split(Line, <<" ">>),
  {binary_to_list(A), binary_to_integer(B)}.

solve(RawData) ->
  Data = parse(RawData),
  {solver(Data, [{0, 0}]), solver(Data, lists:duplicate(9, {0, 0}))}.

solver(Data, Tails) ->
  Reducer =
    fun({D, V}, Acc) ->
       lists:foldl(fun(_, Acc2) ->
                      {HeadPos, TailPos, Visits} = Acc2,
                      move(HeadPos, TailPos, D, Visits)
                   end,
                   Acc,
                   lists:seq(1, V))
    end,
  {_, _, Visits} = lists:foldl(Reducer, {{0, 0}, Tails, []}, Data),
  sets:size(
    sets:from_list(Visits)).

move({HX, HY}, Tails, D, Visits) ->
  HeadPos =
    case D of
      "U" ->
        {HX, HY + 1};
      "D" ->
        {HX, HY - 1};
      "L" ->
        {HX - 1, HY};
      "R" ->
        {HX + 1, HY}
    end,
  NewTails = move_tails(HeadPos, Tails),
  {HeadPos, NewTails, [lists:last(NewTails) | Visits]}.

move_tail({HX, HY}, {TX, TY}) when HX == TX, HY - TY == 2 ->
  {TX, TY + 1};
move_tail({HX, HY}, {TX, TY}) when HX == TX, TY - HY == 2 ->
  {TX, TY - 1};
move_tail({HX, HY}, {TX, TY}) when HY == TY, HX - TX == 2 ->
  {TX + 1, TY};
move_tail({HX, HY}, {TX, TY}) when HY == TY, TX - HX == 2 ->
  {TX - 1, TY};
move_tail({HX, HY}, {TX, TY}) ->
  case {HX - TX, HY - TY} of
    {DX, DY} when abs(DX * DY) =:= 1 ->
      {TX, TY};
    {DX, DY} when DX > 0, DY > 0 ->
      {TX + 1, TY + 1};
    {DX, DY} when DX > 0, DY < 0 ->
      {TX + 1, TY - 1};
    {DX, DY} when DX < 0, DY > 0 ->
      {TX - 1, TY + 1};
    {DX, DY} when DX < 0, DY < 0 ->
      {TX - 1, TY - 1};
    {_, _} ->
      {TX, TY}
  end.

move_tails(_, []) ->
  [];
move_tails(Head, [Tail | Rest]) ->
  NewTail = move_tail(Head, Tail),
  [NewTail | move_tails(NewTail, Rest)].