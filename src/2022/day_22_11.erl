-module(day_22_11).

-export([solve/1]).

solve(Input) ->
  MonkeyInfo = parse(Input),
  {solve1(MonkeyInfo), solve2(MonkeyInfo)}.

solve1(MonkeyInfo) ->
  top_2(rounds(MonkeyInfo, 20, 3, 'div')).

solve2(MonkeyInfo) ->
  RelaxFactor =
    lists:foldl(fun(X, Acc) -> X * Acc end,
                1,
                [maps:get(divisible, X) || {_, X} <- maps:to_list(MonkeyInfo)]),
  top_2(rounds(MonkeyInfo, 10_000, RelaxFactor, 'rem')).

rounds(MonkeyInfo, Count, RelaxFactor, Relaxor) ->
  lists:foldl(fun(_, Acc) -> single_round(Acc, RelaxFactor, Relaxor) end,
              MonkeyInfo,
              lists:seq(1, Count)).

top_2(Rounds) ->
  [{_, #{inspected := A}}, {_, #{inspected := B}} | _] =
    lists:sort(fun({_, X}, {_, Y}) -> maps:get(inspected, X) > maps:get(inspected, Y) end,
               maps:to_list(Rounds)),
  A * B.

single_round(MonkeyInfo, RelaxFactor, Relaxor) ->
  Monkeys = lists:seq(0, map_size(MonkeyInfo) - 1),
  UpdateMonkeyInfo =
    fun(WorryLevel, Target, Source, Map) ->
       maps:update_with(Target,
                        fun(X) -> maps:update_with(items, fun(I) -> I ++ [WorryLevel] end, X) end,
                        maps:update_with(Source,
                                         fun(X) ->
                                            X#{items := [], inspected := maps:get(inspected, X) + 1}
                                         end,
                                         Map))
    end,
  Reducer =
    fun(Idx, AccMonkey) ->
       #{items := Items,
         op := Op,
         divisible := Divisible,
         throw1 := Throw1,
         throw2 := Throw2} =
         maps:get(Idx, AccMonkey),
       UpdatedWorryLevel = [Op(I) || I <- Items],
       AfterRelax = [apply(erlang, Relaxor, [W, RelaxFactor]) || W <- UpdatedWorryLevel],
       lists:foldl(fun(X, Acc) ->
                      case X rem Divisible of
                        0 -> UpdateMonkeyInfo(X, Throw1, Idx, Acc);
                        _ -> UpdateMonkeyInfo(X, Throw2, Idx, Acc)
                      end
                   end,
                   AccMonkey,
                   AfterRelax)
    end,
  lists:foldl(Reducer, MonkeyInfo, Monkeys).

parse(Input) ->
  RawMonkeyInfo = binary:split(Input, <<"\n\n">>, [global]),
  maps:from_list([parse_monkey(X) || X <- RawMonkeyInfo]).

parse_monkey(MonkeyRow) ->
  [<<"Monkey ", Id:8, ":">>,
   <<"  Starting items: ", Items/binary>>,
   <<"  Operation: new = old ", Op/binary>>,
   <<"  Test: divisible by ", Divisible/binary>>,
   <<"    If true: throw to monkey ", Throw1/binary>>,
   <<"    If false: throw to monkey ", Throw2/binary>>] =
    binary:split(MonkeyRow, <<"\n">>, [global]),
  {Id - $0,
   #{items =>
       lists:map(fun(X) -> binary_to_integer(X) end, binary:split(Items, <<", ">>, [global])),
     inspected => 0,
     op => parse_operator(Op),
     divisible => binary_to_integer(Divisible),
     throw1 => binary_to_integer(Throw1),
     throw2 => binary_to_integer(Throw2)}}.

parse_operator(Op) ->
  case binary:split(Op, <<" ">>) of
    [X, <<"old">>] ->
      fun(Old) -> apply(erlang, binary_to_atom(X), [Old, Old]) end;
    [X, C] ->
      fun(Old) -> apply(erlang, binary_to_atom(X), [Old, binary_to_integer(C)]) end
  end.