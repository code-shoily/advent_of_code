-module(day_22_18).

-export([solve/1]).

parse(RawData) ->
  [begin
     [A, B, C] = binary:split(X, <<",">>, [global, trim]),
     {binary_to_integer(A), binary_to_integer(B), binary_to_integer(C)}
   end
   || X <- binary:split(RawData, <<"\n">>, [global, trim])].

solve(RawInput) ->
  Data = sets:from_list(parse(RawInput), [{version, 2}]),
  {solve1(Data), solve2(Data)}.

solve1(Data) ->
  lists:foldl(fun(C, Acc) -> Acc + length(exposed_sides(C, Data)) end,
              0,
              sets:to_list(Data)).

solve2(Data) ->
  Confines = confines(sets:to_list(Data)),
  Exposed = exposed_cubes(Confines, Data),
  lists:foldl(fun(C, Acc) ->
                 ES = exposed_sides(C, Data),
                 Acc + length(ES) - length([E || E <- ES, not sets:is_element(E, Exposed)])
              end,
              0,
              sets:to_list(Data)).

exposed_sides({X, Y, Z}, Map) ->
  Coords =
    [{X - 1, Y, Z},
     {X + 1, Y, Z},
     {X, Y - 1, Z},
     {X, Y + 1, Z},
     {X, Y, Z - 1},
     {X, Y, Z + 1}],
  [C || C <- Coords, not sets:is_element(C, Map)].

is_within_confines({X, Y, Z}, {X1, X2, Y1, Y2, Z1, Z2})
  when X >= X1 - 1, X =< X2 + 1, Y >= Y1 - 1, Y =< Y2 + 1, Z >= Z1 - 1, Z =< Z2 + 1 ->
  true;
is_within_confines(_, _) ->
  false.

exposed_cubes(Confines, Map) ->
  Start = {0, 0, 0},
  exposed_cubes([Start], Confines, Map, sets:new([{version, 2}])).

exposed_cubes([], _, _, Acc) ->
  Acc;
exposed_cubes([C | Rest], Confines, Set, Acc) ->
  ToFollow =
    [E
     || E <- exposed_sides(C, Set),
        not sets:is_element(E, Acc),
        is_within_confines(C, Confines)],
  exposed_cubes(ordsets:union(Rest, ordsets:from_list(ToFollow)),
                Confines,
                Set,
                sets:add_element(C, Acc)).

confines(Data) ->
  {Xs, Ys, Zs} = lists:unzip3(Data),
  MinX = lists:min(Xs),
  MaxX = lists:max(Xs),
  MinY = lists:min(Ys),
  MaxY = lists:max(Ys),
  MinZ = lists:min(Zs),
  MaxZ = lists:max(Zs),
  {MinX, MaxX, MinY, MaxY, MinZ, MaxZ}.
