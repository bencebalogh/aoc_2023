-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    Scores = lists:filtermap(fun ([Winning, Actual]) ->
                                 Won = length(lists:filter(fun (I) -> lists:member(I, Winning) end, Actual)),
                                 case Won of
                                     0 -> false;
                                     _ -> {true, score(Won)}
                                 end
                             end, input()),
    lists:sum(Scores).

part2() ->
    Input = input(),
    Res = lists:foldl(fun ({Index, [Winning, Actual]}, Agg) ->
                          Won = length(lists:filter(fun (I) -> lists:member(I, Winning) end, Actual)),
                          case Won of
                              0 ->
                                  Agg;
                              _ ->
                                  Extra = lists:seq(Index + 1, Index + Won),
                                  Amount = maps:get(Index, Agg, 0) + 1,
                                  lists:foldl(fun (I, A) ->
                                                  maps:update_with(I, fun (C) -> C + Amount end, Amount, A)
                                              end, Agg, Extra)
                          end
                      end, #{}, lists:zip(lists:seq(1, length(Input)), Input)),
    lists:sum(maps:values(Res)) + length(Input).
   
score(Nr) -> math:pow(2, Nr - 1).
   
input() ->
    {ok, I} = file:read_file(input),
    Lines = binary:split(I, <<"\n">>, [global]),
    lists:map(fun (Line) ->
                  [<<"Card ", _Id/binary>> , Nrs] = binary:split(Line, <<": ">>),
                  S = binary:split(Nrs, <<" | ">>),
                  lists:map(fun (Si) ->
                                lists:map(fun binary_to_integer/1, binary:split(Si, <<" ">>, [global, trim_all]))
                            end, S)
              end, Lines).