-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    lists:sum(lists:map(fun next/1, input())).
    
part2() ->
    lists:sum(lists:map(fun next2/1, input())).

next(Nrs) ->
    next(Nrs, [], [Nrs], part1).

next2(Nrs) ->
    next(Nrs, [], [Nrs], part2).

next([H1 | [H2 | _] = R], Diffs, Rows, Part) ->
    next(R, [H2 - H1 | Diffs], Rows, Part);
next(_, Diffs, Rows, Part) ->
    D = lists:reverse(Diffs),
    R = [D | Rows],
    case ordsets:from_list(Diffs) of
        [_] ->
            next_value(R, 0, Part);
        _ ->
            next(D, [], R, Part)
    end.

next_value([], Acc, _Part) ->
    Acc;
next_value([Previous | Rest], Acc, Part) ->
    P = hd(maybe_reverse(Previous, Part)),
    next_value(Rest, calc(P, Acc, Part), Part).

maybe_reverse(Previous, part1) ->
    lists:reverse(Previous);
maybe_reverse(Previous, part2) ->
    Previous.

calc(P, Acc, part1) ->
    P + Acc;
calc(P, Acc, part2) ->
    P - Acc.

input() ->
    {ok, I} = file:read_file(input),
    Lines = binary:split(I, <<"\n">>, [global]),
    lists:map(fun (Line) -> lists:map(fun binary_to_integer/1, binary:split(Line, <<" ">>, [global])) end, Lines).