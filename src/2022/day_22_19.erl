-module(day_22_19).

-export([solve/1]).

parse(RawData) ->
    [
        get_integers(X)
     || X <- binary:split(RawData, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global, trim])
    ].

solve(RawInput) ->
    Data = parse(RawInput),
    {solve1(Data), solve2(Data)}.

get_integers(Bin) ->
    {match, Match} = re:run(Bin, <<"([-\\d]+)">>, [{capture, all_but_first, binary}, global]),
    [binary_to_integer(X) || [X] <- Match].

solve1(Data) ->
    lists:sum(
        lists:map(fun quality/1, Data)
    ).

solve2(Data) ->
    {L, _} = lists:split(3, Data),
    lists:foldl(fun erlang:'*'/2, 1, [max_geodes(tl(X), 32) || X <- L]).

quality([Id | Blueprint]) ->
    Id * max_geodes(Blueprint, 24).

max_geodes(Blueprint, N) ->
    Start = {{1, 0, 0, 0}, {0, 0, 0, 0}},
    [OreP, OreClayP, OreObsP, _, OreGeodeP, _] = Blueprint,
    max_geodes([Start], {Blueprint, lists:max([OreP, OreClayP, OreObsP, OreGeodeP])}, N).

max_geodes(Options, _Blueprint, 0) ->
    Max = lists:max([X || {_, {_, _, _, X}} <- Options]),
    Max;
max_geodes(Options, Blueprint, TimeLeft) ->
    ToFollow = [X || M <- Options, X <- moves(M, Blueprint, TimeLeft)],
    max_geodes(
        sets:to_list(
            sets:from_list(ToFollow, [{version, 2}])
        ),
        Blueprint,
        TimeLeft - 1
    ).

moves(
    {{OreRobotN, ClayRobotN, ObsRobotN, GeodeRobotN} = Robots, {Ore, Clay, Obs, Geode}},
    {[OreP, OreClayP, OreObsP, ClayObsP, OreGeodeP, ObsGeodeP], OreMax},
    TimeLeft
) ->
    OreLimit = TimeLeft * OreMax - OreRobotN * (TimeLeft - 1),
    ClayLimit = TimeLeft * ClayObsP - ClayRobotN * (TimeLeft - 1),
    ObsLimit = TimeLeft * ObsGeodeP - ObsRobotN * (TimeLeft - 1),
    NewItems =
        {
            min(Ore, OreLimit) + OreRobotN,
            min(Clay, ClayLimit) + ClayRobotN,
            min(Obs, ObsLimit) + ObsRobotN,
            Geode + GeodeRobotN
        },
    O1 = [{Robots, NewItems}],
    O2 =
        [
            {plus(Robots, {1, 0, 0, 0}), plus({-OreP, 0, 0, 0}, NewItems)}
         || Ore >= OreP, OreRobotN < OreMax
        ],
    O3 =
        [
            {plus(Robots, {0, 1, 0, 0}), plus({-OreClayP, 0, 0, 0}, NewItems)}
         || Ore >= OreClayP, ClayRobotN < ClayObsP
        ],
    O4 =
        [
            {plus(Robots, {0, 0, 1, 0}), plus({-OreObsP, -ClayObsP, 0, 0}, NewItems)}
         || Ore >= OreObsP, Clay >= ClayObsP, ObsRobotN < ObsGeodeP
        ],
    O5 =
        [
            {plus(Robots, {0, 0, 0, 1}), plus({-OreGeodeP, 0, -ObsGeodeP, 0}, NewItems)}
         || Ore >= OreGeodeP, Obs >= ObsGeodeP
        ],
    case Ore >= OreMax + 1 of
        true ->
            O2 ++ O3 ++ O4 ++ O5;
        false ->
            O1 ++ O2 ++ O3 ++ O4 ++ O5
    end.

plus({A1, B1, C1, D1}, {A2, B2, C2, D2}) ->
    {A2 + A1, B2 + B1, C2 + C1, D1 + D2}.
