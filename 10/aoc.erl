-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    Grid = input(),
    Start = get_start_position(Grid),
    StartPipe = get_start_pipe(Start, Grid),
    [D1, D2] = connects_to(Start, StartPipe),
    Direction1 = walk_loop(D1, Grid, #{Start => 0}, 1),
    Direction2 = walk_loop(D2, Grid, #{Start => 0}, 1),
    Loop = maps:merge_with(fun (_, L1, L2) -> min(L1, L2) end, Direction1, Direction2),
    lists:max(maps:values(Loop)).
    
part2() ->
    Grid = input(),
    Start = get_start_position(Grid),
    StartPipe = get_start_pipe(Start, Grid),
    [D1, _] = connects_to(Start, StartPipe),
    Loop = walk_loop(D1, Grid, #{Start => 0}, 1),
    count_tiles(Loop, Grid, Start, StartPipe).

walk_loop(Position, Grid, Path, Steps) ->
    Conn = connects_to(Position, maps:get(Position, Grid)),
    case lists:filter(fun (N) -> not maps:is_key(N, Path) end, Conn) of
        [] -> Path;
        [Next] -> walk_loop(Next, Grid, maps:put(Position, Steps, Path), Steps + 1)
    end.

count_tiles(Loop, Grid, Start, StartPipe) ->
    CleanGrid = maps:fold(fun
                              (Key, _, G) when is_map_key(Key, Loop) ->
                                   maps:put(Key, maps:get(Key, Grid), G);
                              (Key, _, G) ->
                                   maps:put(Key, $., G)
                          end, #{}, Grid),
    count_tiles(maps:put(Start, StartPipe, CleanGrid)).

count_tiles(Grid) ->
    MaxY = lists:max(lists:map(fun ({_, Y}) -> Y end, maps:keys(Grid))),
    count_row(0, 0, MaxY, Grid).

count_row(Count, MaxY, MaxY, _Grid) ->
    Count;
count_row(Count, Y, MaxY, Grid) ->
    Inside = maps:get({0, Y}, Grid) /= $.,
    NewCount = check_row(Grid, 0, Y, Count, Inside, null),
    count_row(NewCount, Y + 1, MaxY, Grid).

check_row(Grid, X, Y, Count, Inside, Opener) ->
    case maps:get({X, Y}, Grid, oob) of
        oob ->
            Count;
        $. ->
            case Inside of
                true -> check_row(Grid, X + 1, Y, Count + 1, Inside, Opener);
                false -> check_row(Grid, X + 1, Y, Count, Inside, Opener)
            end;
        $| ->
            check_row(Grid, X + 1, Y, Count, not Inside, Opener);
        $L ->
            check_row(Grid, X + 1, Y, Count, Inside, $L);
        $F ->
            check_row(Grid, X + 1, Y, Count, Inside, $F);
        $7 ->
            case Opener of
                $F -> check_row(Grid, X + 1, Y, Count, Inside, null);
                _ -> check_row(Grid, X + 1, Y, Count, not Inside, null)
            end;
        $J ->
            case Opener of
                $L -> check_row(Grid, X + 1, Y, Count, Inside, null);
                _ -> check_row(Grid, X + 1, Y, Count, not Inside, null)
            end;
        $- ->
            check_row(Grid, X + 1, Y, Count, Inside, Opener)

    end.

get_start_position(Grid) ->
    {value, {Start, $S}} = lists:search(fun ({_, $S}) -> true; (_) -> false end, maps:to_list(Grid)),
    Start.

get_start_pipe({X, Y}, Grid) ->
    Neighbours = [{X + 1, Y}, {X - 1, Y}, {X, Y - 1}, {X, Y + 1}],
    Connects = lists:filter(fun (N) ->
                                lists:member({X, Y}, connects_to(N, maps:get(N, Grid)))
                            end, Neighbours),
    Options = [$|, $-, $L, $J, $7, $7, $F],
    {value, Pipe} = lists:search(fun (O) ->
                                     ordsets:from_list(connects_to({X, Y}, O)) == ordsets:from_list(Connects)
                                 end, Options),
    Pipe.


connects_to({X, Y}, $|) ->
    [{X, Y - 1}, {X, Y + 1}];
connects_to({X, Y}, $-) ->
    [{X - 1, Y}, {X + 1, Y}];
connects_to({X, Y}, $L) ->
    [{X, Y - 1}, {X + 1, Y}];
connects_to({X, Y}, $J) ->
    [{X, Y - 1}, {X - 1, Y}];
connects_to({X, Y}, $7) ->
    [{X, Y + 1}, {X - 1, Y}];
connects_to({X, Y}, $F) ->
    [{X, Y + 1}, {X + 1, Y}];
connects_to({_X, _Y}, $.) ->
    [].

input() ->
    {ok, I} = file:read_file(input),
    Lines = binary:split(I, <<"\n">>, [global]),
    {P, _} = lists:foldl(fun (Line, {Agg, Y}) ->
                              {NewGrid, _} = lists:foldl(fun (Char, {G, X}) ->
                                                          {maps:put({X, Y}, Char, G), X + 1}
                                                      end, {Agg, 0}, binary_to_list(Line)),
                              {NewGrid, Y + 1}
                          end, {#{}, 0}, Lines),
    P.