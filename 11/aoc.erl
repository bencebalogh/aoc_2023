-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    run(1).
    
part2() ->
    run(1_000_000 - 1).

run(ExpansionRate) ->
    {Galaxies, SizeY, SizeX} = input(),
    E1 = expand(Galaxies, {SizeX, SizeY}, 1, ExpansionRate),
    Expanded = expand(E1, {SizeX, SizeY}, 2, ExpansionRate),
    sum_path(Expanded, 0).

sum_path([_Last], Sum) ->
    Sum;
sum_path([{X1, Y1} | Rest], Sum) ->
    NewSum = lists:foldl(fun ({X2, Y2}, S) -> S + abs(X2 - X1) + abs(Y2 - Y1) end, Sum, Rest),
    sum_path(Rest, NewSum).

expand(Galaxies, {SizeX, SizeY}, Element, Rate) ->
    Checking = element(Element, {SizeX, SizeY}),
    Empties = lists:filter(fun (C) ->
                               lists:all(fun (Galaxy) ->
                                           element(Element, Galaxy) /= C
                                       end, Galaxies)
                           end, lists:seq(1, Checking)),
    Add = lists:foldl(fun (Empty, Agg) ->
                          lists:foldl(fun (Galaxy, A) ->
                                          case element(Element, Galaxy) of
                                              I when I > Empty ->
                                                maps:update_with(Galaxy, fun (P) -> P + 1 end, 1, A);
                                              _ ->
                                                maps:update_with(Galaxy, fun (P) -> P end, 0, A)
                                          end
                                      end, Agg, Galaxies)
                      end, #{}, Empties),
    lists:foldl(fun ({Galaxy, Plus}, NewGalaxies) ->
                    [add_to(Galaxy, Element, Plus, Rate) | NewGalaxies]
                end, [], maps:to_list(Add)).

add_to({X, Y}, 1, Add, Rate) -> {X + (Add * Rate), Y};
add_to({X, Y}, 2, Add, Rate) -> {X, Y + (Add * Rate)}.

input() ->
    {ok, I} = file:read_file(input),
    Lines = binary:split(I, <<"\n">>, [global]),
    {P, R} = lists:foldl(fun (Line, {Agg, Y}) ->
                              {NewGrid, _} = lists:foldl(fun ($., {G, X}) ->
                                                                 {G, X + 1};
                                                             ($#, {G, X}) ->
                                                                 {[{X, Y} | G], X + 1}
                                                         end, {Agg, 1}, binary_to_list(Line)),
                              {NewGrid, Y + 1}
                          end, {[], 1}, Lines),
    {P, R, size(hd(Lines))}.