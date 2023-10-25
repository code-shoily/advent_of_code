-module(day_22_21).

-export([solve/1]).

parse(RawInput) ->
    maps:from_list([
        parse_monkey(X)
     || X <- binary:split(RawInput, [<<"\r">>, <<"\n">>, <<"\r\n">>], [global, trim])
    ]).

solve(Input) ->
    Data = parse(Input),
    {solve1(Data), solve2(Data)}.

parse_monkey(<<Name:4/binary, ": ", Rest/binary>>) ->
    {Name, parse_monkey_value(Rest)}.

parse_monkey_value(<<N1:4/binary, " ", Op:1/binary, " ", N2:4/binary>>) ->
    {N1, Op, N2};
parse_monkey_value(V) ->
    binary_to_integer(V).

solve1(Data) ->
    stack(<<"root">>, Data).

solve2(Data0) ->
    Data = maps:remove(<<"humn">>, Data0),
    {R1, _, R2} = maps:get(<<"root">>, Data),
    S1 = stack(R1, Data),
    S2 = stack(R2, Data),
    inverse(S1, S2).

stack(M, Data) ->
    case maps:get(M, Data, null) of
        null ->
            null;
        Integer when is_integer(Integer) ->
            Integer;
        {M1, Op, M2} ->
            S1 = stack(M1, Data),
            S2 = stack(M2, Data),
            case is_integer(S1) andalso is_integer(S2) of
                true ->
                    op(Op, S1, S2);
                false ->
                    {Op, S1, S2}
            end
    end.

op(<<"+">>, X, Y) ->
    X + Y;
op(<<"-">>, X, Y) ->
    X - Y;
op(<<"*">>, X, Y) ->
    X * Y;
op(<<"/">>, X, Y) ->
    X div Y.

inverse(S1, V) when is_integer(V) ->
    inverse(V, S1);
inverse(V, {Op, V1, V2}) ->
    case Op of
        <<"+">> when is_integer(V1) ->
            inverse(V - V1, V2);
        <<"+">> ->
            inverse(V - V2, V1);
        <<"*">> when is_integer(V1) ->
            inverse(V div V1, V2);
        <<"*">> ->
            inverse(V div V2, V1);
        <<"-">> when is_integer(V1) ->
            inverse(V1 - V, V2);
        <<"-">> ->
            inverse(V + V2, V1);
        <<"/">> when is_integer(V1) ->
            inverse(V1 div V, V2);
        <<"/">> ->
            inverse(V * V2, V1)
    end;
inverse(V, null) ->
    V.
