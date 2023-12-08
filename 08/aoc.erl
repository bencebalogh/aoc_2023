-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    {Steps, Nodes} = input(),
    walk(Steps, Nodes, 0, <<"AAA">>, Steps).
    
part2() ->
    {Steps, Nodes} = input(),
    StartingNodes = lists:filter(fun (<<_:2/binary, E:1/binary>>) -> E == <<"A">> end, maps:keys(Nodes)),
    Shortests = lists:map(fun (Start) -> ghost_walk(Steps, Nodes, 0, Start, Steps) end, StartingNodes),
    lists:foldl(fun (C, Agg) -> lcm(C, Agg) end, 1, Shortests).

walk([], Nodes, Count, Current, OriginalSteps) ->
    walk(OriginalSteps, Nodes, Count, Current, OriginalSteps);
walk(_, _Nodes, Count, <<"ZZZ">>, _OriginalSteps) ->
    Count;
walk([Step | Rest], Nodes, Count, Current, OriginalSteps) ->
    #{Step := Next} = maps:get(Current, Nodes),
    walk(Rest, Nodes, Count + 1, Next, OriginalSteps).

ghost_walk([], Nodes, Count, Current, OriginalSteps) ->
    ghost_walk(OriginalSteps, Nodes, Count, Current, OriginalSteps);
ghost_walk(_, _Nodes, Count, <<_:2/binary, "Z">>, _OriginalSteps) ->
    Count;
ghost_walk([Step | Rest], Nodes, Count, Current, OriginalSteps) ->
    #{Step := Next} = maps:get(Current, Nodes),
    ghost_walk(Rest, Nodes, Count + 1, Next, OriginalSteps).

gcd(A, B) when A < B ->
    gcd(B, A);
gcd(A, B) when A rem B == 0 ->
    B;
gcd(A, B) ->
    gcd(B, A rem B).

lcm(A, B) ->
    (A * B) div gcd(A, B).

input() ->
    {ok, I} = file:read_file(input),
    [Steps, NodesRaw] = binary:split(I, <<"\n\n">>, [global]),
    Nodes = lists:foldl(fun (N, Map) ->
                            {match, [[Name], [Left], [Right]]} = re:run(N, "(\\w{3})", [global, {capture, [1], binary}]),
                            maps:put(Name, #{$L => Left, $R => Right}, Map)
                        end, #{}, binary:split(NodesRaw, <<"\n">>, [global])),
    {binary_to_list(Steps), Nodes}.