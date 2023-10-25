-module(day_22_10).

-export([solve/1]).

-define(print, "█").
-define(blank, "▒").

solve(RawData) ->
    {Cycles, _, _} = run(parse(RawData)),
    Nths = [20, 60, 100, 140, 180, 220],
    Solve1 = lists:sum([X * element(2, lists:keyfind(X, 1, Cycles)) || X <- Nths]),
    Solve2 =
        lists:foreach(fun render/1, lists:sort(fun({X, _}, {Y, _}) -> X < Y end, Cycles)),
    {Solve1, Solve2}.

parse(RawInstructions) ->
    [
        parse_instruction(RawInstruction)
     || RawInstruction <- binary:split(RawInstructions, [<<"\r">>, <<"\n">>, <<"\r\n">>], [
            global, trim
        ])
    ].

parse_instruction(<<"noop">>) ->
    noop;
parse_instruction(<<"addx ", Val/binary>>) ->
    binary_to_integer(Val).

run(Instructions) ->
    lists:foldl(
        fun
            (noop, {Xs, Cycle, Value}) ->
                {[{Cycle, Value} | Xs], Cycle + 1, Value};
            (AddValue, {Xs, Cycle, Value}) when is_integer(AddValue) ->
                {[{Cycle, Value}, {Cycle + 1, Value} | Xs], Cycle + 2, Value + AddValue}
        end,
        {[], 1, 1},
        Instructions
    ).

render({Cycle, X}) ->
    Shift = Cycle - 40 * (Cycle div 40) - 1,
    if
        Shift >= X - 1 andalso Shift =< X + 1 ->
            io:format(?print);
        true ->
            io:format(?blank)
    end,
    if
        Cycle rem 40 == 0 ->
            io:format("\n");
        true ->
            ok
    end.
