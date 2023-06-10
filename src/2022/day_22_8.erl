-module(day_22_8).

-export([solve/1]).

parse(RawHeights) ->
  TreeList =
    [[X - $0 || X <- binary_to_list(Line)]
     || Line <- binary:split(RawHeights, <<"\n">>, [global, trim])],
  Length = length(TreeList),
  Breadth = length(hd(TreeList)),
  Edges = 2 * Length + 2 * Breadth - 4,
  TreeMap = tree_list_to_tree_map(TreeList),
  {TreeMap, Edges, Length, Breadth}.

solve(RawHeights) ->
  {TreeMap, Edges, Length, Breadth} = parse(RawHeights),
  Solve1 = solve1(TreeMap, Edges, Length, Breadth),
  Solve2 = solve2(TreeMap, Length, Breadth),
  {Solve1, Solve2}.

solve1(TreeMap, Edges, Length, Breadth) ->
  Interior =
    length([true
            || X <- lists:seq(2, Length - 1),
               Y <- lists:seq(2, Breadth - 1),
               is_visible({X, Y}, {Length, Breadth}, TreeMap)]),
  Edges + Interior.

solve2(TreeMap, Length, Breadth) ->
  lists:max([scenic_score({X, Y}, {Length, Breadth}, TreeMap)
             || X <- lists:seq(2, Length - 1), Y <- lists:seq(2, Breadth - 1)]).

tree_list_to_tree_map(TreeList) ->
  tree_list_to_tree_map(TreeList, 1, 1, []).

tree_list_to_tree_map([], _, _, TreeList) ->
  maps:from_list(TreeList);
tree_list_to_tree_map([[] | Lengths], X, _, TreeList) ->
  tree_list_to_tree_map(Lengths, X + 1, 1, TreeList);
tree_list_to_tree_map([[Height | Breadths] | Lengths], X, Y, TreeList) ->
  tree_list_to_tree_map([Breadths | Lengths], X, Y + 1, [{{X, Y}, Height} | TreeList]).

seen_from(top, {XX, YY}, _) ->
  [{X, YY} || X <- lists:seq(XX - 1, 1, -1)];
seen_from(bottom, {XX, YY}, {Length, _}) ->
  [{X, YY} || X <- lists:seq(XX + 1, Length)];
seen_from(left, {XX, YY}, _) ->
  [{XX, Y} || Y <- lists:seq(YY - 1, 1, -1)];
seen_from(right, {XX, YY}, {_, Breadth}) ->
  [{XX, Y} || Y <- lists:seq(YY + 1, Breadth)].

is_visible(Coord, Dims, Map) ->
  lists:any(fun(Dir) -> is_tallest(maps:get(Coord, Map), Map, seen_from(Dir, Coord, Dims))
            end,
            [top, bottom, left, right]).

is_tallest(Height, Map, Coords) ->
  lists:all(fun(C) -> maps:get(C, Map) < Height end, Coords).

scenic_score(Coord, Dims, Map) ->
  Height = maps:get(Coord, Map),
  Reducer =
    fun(Dir, Acc) -> Acc * view_distance(Height, Map, seen_from(Dir, Coord, Dims)) end,
  lists:foldl(Reducer, 1, [top, bottom, left, right]).

view_distance(Height, TreeMap, [C | Rest]) ->
  case maps:get(C, TreeMap) >= Height of
    true ->
      1;
    false ->
      1 + view_distance(Height, TreeMap, Rest)
  end;
view_distance(_, _, []) ->
  0.