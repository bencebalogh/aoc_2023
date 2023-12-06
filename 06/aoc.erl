-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    winnings(input(), 1).
    
part2() ->
    [Times, Distances] = input(),
    winnings([[concat(Times)], [concat(Distances)]], 1).

winnings([[], []], Winnings) ->
    Winnings;
winnings([[Time | TimeRest], [Distance | DistanceRest]], Winnings) ->
    Times = lists:seq(1, Time - 1),
    Won = lists:filter(fun (B) -> B * (Time - B) > Distance end, Times),
    NewWinnings = Winnings * length(Won),
    winnings([TimeRest, DistanceRest], NewWinnings).

concat(Nrs) ->
    B = lists:foldl(fun (I, Agg) ->
                        T = integer_to_binary(I),
                        <<Agg/binary, T/binary>>
                    end, <<>>, Nrs),
    binary_to_integer(B).

input() ->
    {ok, I} = file:read_file(input),
    Raw = binary:split(I, <<"\n">>, [global]),
    lists:map(fun (R) ->
                  [_, Nrs] = binary:split(R, <<":">>, [global]),
                  Nrsb = binary:split(Nrs, <<" ">>, [global, trim_all]),
                  lists:map(fun binary_to_integer/1, Nrsb)
              end, Raw).
