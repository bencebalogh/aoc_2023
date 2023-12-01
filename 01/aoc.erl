-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    run(false).

part2() ->
    run(true).

run(Transform) ->
    Nrs = lists:map(fun (Line) ->
                        ToCheck = maybe_transform(Transform, Line),
                        case re:replace(ToCheck, <<"[a-z]">>, <<"">>,[global, {return, list}]) of
                            [A] ->
                                list_to_integer([A, A]);
                            [A | Rest] ->
                                [B | _] = lists:reverse(Rest),
                                list_to_integer([A, B])
                        end
                    end, input()),
    lists:sum(Nrs).

maybe_transform(true, Line) -> transform(Line);
maybe_transform(false, Line) -> Line.

transform(Line) ->
    Transformations = [
        {<<"one">>, <<"o1e">>},
        {<<"two">>, <<"t2o">>},
        {<<"three">>, <<"t3e">>},
        {<<"four">>, <<"f4r">>},
        {<<"five">>, <<"f5e">>},
        {<<"six">>, <<"s6x">>},
        {<<"seven">>, <<"s7n">>},
        {<<"eight">>, <<"e8t">>},
        {<<"nine">>, <<"n9e">>}
    ],
    lists:foldl(fun ({From, To}, L) ->
                    binary:replace(L, From, To, [global])
                end, Line, Transformations).

input() ->
    {ok, I} = file:read_file(input),
    binary:split(I, <<"\n">>, [global]).
