-module(day_22_24).

-export([solve/1]).

parse(RawData) ->
    Data =
        [
            [X || X <- binary_to_list(Line)]
         || Line <- binary:split(RawData, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global, trim])
        ],
    maps:from_list([
        {
            {X, Y},
            case N of
                $. ->
                    [];
                _ ->
                    [N]
            end
        }
     || {Y, Line} <- lists:enumerate(Data),
        {X, N} <- lists:enumerate(Line),
        N =/= $#
    ]).

solve(RawInput) ->
    Map = parse(RawInput),
    {solve1(Map), solve2(Map)}.

solve1(Map) ->
    Start = [{0, {2, 1}}],
    End =
        {MaxX, MaxY} =
        lists:max(
            maps:keys(Map)
        ),
    {Best, _} = search(Start, End, {MaxX, MaxY - 1}, #{0 => Map}),
    Best.

solve2(Map) ->
    Start = {2, 1},
    End =
        {MaxX, MaxY} =
        lists:max(
            maps:keys(Map)
        ),
    MaxCoord = {MaxX, MaxY - 1},
    {Best1, Map1} = search([{0, Start}], End, MaxCoord, #{0 => Map}),
    {Best2, Map2} = search([{Best1, End}], Start, MaxCoord, #{Best1 => Map1}),
    {Best3, _} = search([{Best2, Start}], End, MaxCoord, #{Best2 => Map2}),
    Best3.

simulate(Map, Maxs) ->
    maps:fold(fun(K, V, Acc) -> fold_blizzard(K, V, Maxs, Acc) end, #{}, Map).

fold_blizzard(_, [], {_, _}, Acc) ->
    Acc;
fold_blizzard(C, Dirs, {MaxX, MaxY}, Acc) ->
    lists:foldl(fun(D, Acc0) -> do_move_blizzard(D, C, {MaxX, MaxY}, Acc0) end, Acc, Dirs).

do_move_blizzard(Dir, {X, Y}, {MaxX, MaxY}, Map) ->
    NewC =
        case Dir of
            $> when X < MaxX ->
                {X + 1, Y};
            $> ->
                {2, Y};
            $< when X > 2 ->
                {X - 1, Y};
            $< ->
                {MaxX, Y};
            $v when Y < MaxY ->
                {X, Y + 1};
            $v ->
                {X, 2};
            $^ when Y > 2 ->
                {X, Y - 1};
            $^ ->
                {X, MaxY}
        end,
    maps:update_with(NewC, fun(V) -> [Dir | V] end, [Dir], Map).

search(Start, End, MaxCoord, Maps) ->
    search(Start, End, MaxCoord, #{}, inf, Maps).

search([], _, _, _Seen, Best, Maps) ->
    {Best, maps:get(Best, Maps)};
search([{Len, End} | Rest], End, MaxCoord, Seen, Best, Maps) when Len < Best ->
    search(Rest, End, MaxCoord, Seen, Len, Maps);
search([{Len, Coord} | Rest], End, MaxCoord, Seen, Best, Maps) ->
    case Len >= Best of
        true ->
            search(Rest, End, MaxCoord, Seen, Best, Maps);
        false ->
            NewSeen = Seen#{{Len, Coord} => ok},
            NewMaps =
                case maps:is_key(Len + 1, Maps) of
                    false ->
                        Maps#{Len + 1 => simulate(maps:get(Len, Maps), MaxCoord)};
                    true ->
                        Maps
                end,
            ToFollow =
                lists:sort([
                    {Len + 1, C}
                 || C <- adjacent_coords(Coord, MaxCoord, End),
                    not maps:is_key({Len + 1, C}, Seen),
                    not maps:is_key(C, maps:get(Len + 1, NewMaps))
                ]),
            search(ordsets:union(ToFollow, Rest), End, MaxCoord, NewSeen, Best, NewMaps)
    end.

adjacent_coords({X, Y}, _, {EndX, EndY}) when
    X =:= EndX, Y =:= EndY - 1; X =:= EndX, Y =:= EndY + 1
->
    [{X, EndY}];
adjacent_coords({X, Y}, {MaxX, MaxY}, _) ->
    MaybeWait =
        case {X, Y} of
            {2, 1} ->
                [{X, Y}];
            {X, Y} when Y =:= MaxY + 1 ->
                [{X, Y}];
            _ ->
                []
        end,
    [
        {A, B}
     || {A, B} <- [{X + 1, Y}, {X, Y + 1}, {X - 1, Y}, {X, Y - 1}, {X, Y}],
        A =< MaxX,
        B =< MaxY,
        A >= 2,
        B >= 2
    ] ++
        MaybeWait.
