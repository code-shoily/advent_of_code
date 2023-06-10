-module(day_22_15).

-export([solve/1]).

-define(re, <<"([-\\d]+)">>).

parse(RawData) ->
    [begin
         {_, Match} = re:run(Line, ?re, [{capture, all_but_first, binary}, global]),
         [Xs, Ys, Xb, Yb] = [binary_to_integer(N) || [N] <- Match],
         {Sensor, Beacon} = {{Xs, Ys}, {Xb, Yb}},
         {Sensor, Beacon, distance(Sensor, Beacon)}
     end
     || Line <- binary:split(RawData, <<"\n">>, [global, trim])].

solve(RawInput) ->
    Input = parse(RawInput),
    {solve1(Input), solve2(Input)}.

solve1(Input) ->
    ordsets:size(
        ordsets:union([empty_positions(X, 2_000_000) || X <- Input])).

solve2(Input) ->
    tuning_freq(empty_coordinate(0, 0, Input, 4_000_000)).

empty_coordinate(X, Y, Map, Limit) when X =< Limit, Y =< Limit ->
    case first_edge(X, Y, Map) of
        {ok, NewX} when NewX >= Limit ->
            empty_coordinate(0, Y + 1, Map, Limit);
        {ok, NewX} ->
            empty_coordinate(NewX + 1, Y, Map, Limit);
        false ->
            {X, Y}
    end.

first_edge(_, _, []) ->
    false;
first_edge(X, Y, [{{Xs, Ys}, _, Distance} | Rest]) ->
    case distance({Xs, Ys}, {X, Y}) > Distance of
        true ->
            first_edge(X, Y, Rest);
        false ->
            {ok, Xs + (Distance - abs(Ys - Y))}
    end.

empty_positions({{Xs, Ys}, {Xb, Yb}, Distance}, Y) ->
    YDistance = abs(Ys - Y),
    case YDistance > Distance of
        true ->
            [];
        false ->
            S0 = lists:seq(Xs - (Distance - YDistance), Xs + (Distance - YDistance)),
            case Yb of
                Y ->
                    S0 -- [Xb];
                _ ->
                    S0
            end
    end.

distance({X1, Y1}, {X2, Y2}) ->
    abs(X1 - X2) + abs(Y1 - Y2).

tuning_freq({X, Y}) ->
    X * 4_000_000 + Y.
