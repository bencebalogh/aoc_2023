-module(aoc).
-export([part1/0]).
-export([part2/0]).

-define(CUBES, #{red => 12, green => 13, blue => 14}).

part1() ->
    ValidIds = lists:filtermap(fun ({Id, Draws}) ->
                                    case valid(Draws) of
                                        true -> {true, Id};
                                        false -> false
                                    end
                                end, input()),
    lists:sum(ValidIds).

part2() ->
    Powers = lists:map(fun ({_Id, Draws}) -> power(min_cubes(Draws)) end, input()),
    lists:sum(Powers).

valid(Draws) ->
    lists:all(fun (D) -> lists:all(fun ({Color, Amount}) -> maps:get(Color, ?CUBES) >= Amount end, D) end, Draws).

min_cubes(Draws) ->
    % not accurate min cubes, but power calculation is not affected
    min_cubes(Draws, #{green => 1, red => 1, blue => 1}).

min_cubes([], Mins) ->
    Mins;
min_cubes([Draw | Rest], Mins) ->
    NewMins = lists:foldl(fun
                             ({Color, Amount}, N) ->
                                case maps:get(Color, N) of
                                    A when A < Amount -> N#{Color := Amount};
                                    _ -> N
                                end
                          end, Mins, Draw),
    min_cubes(Rest, NewMins).


power(#{red := Red, green := Green, blue := Blue}) -> Red * Green * Blue.

input() ->
    {ok, I} = file:read_file(input),
    Lines = binary:split(I, <<"\n">>, [global]),
    {Parsed, _} = lists:foldl(fun (Line, {Agg, Index}) ->
                                  [_, D] = binary:split(Line, <<": ">>),
                                  Ds = binary:split(D, <<"; ">>, [global]),
                                  R = lists:map(fun (A) ->
                                                    B = binary:split(A, <<", ">>, [global]),
                                                    lists:map(fun (C) ->
                                                                  [Amount, Color] = binary:split(C, <<" ">>),
                                                                  {binary_to_atom(Color), binary_to_integer(Amount)}
                                                              end, B)
                                                end, Ds),
                                  {[{Index, R} | Agg], Index + 1}
                              end, {[], 1}, Lines),
    lists:reverse(Parsed).
