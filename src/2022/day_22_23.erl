-module(day_22_23).

-export([solve/1]).

parse(RawData) ->
    Data =
        [
            [N =:= $# || N <- binary_to_list(Line)]
         || Line <- binary:split(RawData, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global, trim])
        ],
    maps:from_list([
        {{X, Y}, N}
     || {Y, Line} <- lists:enumerate(Data), {X, N} <- lists:enumerate(Line), N
    ]).

solve(RawInput) ->
    Input = parse(RawInput),
    {solve1(Input), solve2(Input)}.

solve1(Map) ->
    NewMap = simulate(Map, 10),
    {Xs, Ys} =
        lists:unzip(
            maps:keys(NewMap)
        ),
    (lists:max(Xs) - lists:min(Xs) + 1) * (lists:max(Ys) - lists:min(Ys) + 1) -
        maps:size(NewMap).

solve2(Map) ->
    simulate(Map, -1).

simulate(Map, N) ->
    Order =
        [
            [{X, -1} || X <- [-1, 0, 1]],
            [{X, 1} || X <- [-1, 0, 1]],
            [{-1, Y} || Y <- [-1, 0, 1]],
            [{1, Y} || Y <- [-1, 0, 1]]
        ],
    iteration({Map, Order}, N).

iteration({Map, _}, 0) ->
    Map;
iteration({Map, Order}, N) ->
    Elves = maps:keys(Map),
    Proposals = [propose(E, Order, Map) || E <- Elves],
    Moves = maps:groups_from_list(fun({To, _From}) -> To end, Proposals),
    NewMap = new_map(Moves),
    case N < 0 andalso NewMap =:= Map of
        true ->
            -N;
        false ->
            iteration({NewMap, tl(Order) ++ [hd(Order)]}, N - 1)
    end.

new_map(List) ->
    lists:foldl(fun new_map/2, #{}, maps:to_list(List)).

new_map({C, [_]}, Map) ->
    Map#{C => true};
new_map({_, [_, _ | _] = List}, Map) ->
    lists:foldl(fun({_, From}, Acc) -> Acc#{From => true} end, Map, List).

propose(C, Order, Map) ->
    S = lists:seq(-1, 1),
    Around = [{X, Y} || X <- S, Y <- S, {X, Y} =/= {0, 0}],
    case have_elves(C, Around, Map) of
        false ->
            {C, C};
        true ->
            do_propose(C, Order, Map)
    end.

do_propose(C, [], _Map) ->
    {C, C};
do_propose(C, [O | Order], Map) ->
    case have_elves(C, O, Map) of
        false ->
            {plus(C, lists:nth(2, O)), C};
        true ->
            do_propose(C, Order, Map)
    end.

have_elves(C, List, Map) when is_list(List) ->
    lists:any(fun(D) -> maps:is_key(plus(C, D), Map) end, List).

plus({X, Y}, {A, B}) ->
    {X + A, Y + B}.
