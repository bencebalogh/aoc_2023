-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    run(fun sort/2).
    
part2() ->
    run(fun sort2/2).

run(SortFn) ->
    Sorted = lists:sort(SortFn, input()),
    Wins = lists:map(fun ({I, {_, Bet}}) -> I * Bet end, lists:zip(lists:seq(1, length(Sorted)), Sorted)),
    lists:sum(Wins).

sort({Hand1, _Bet1}, {Hand2, _Bet2}) ->
    case {power(lists:sort(Hand1)), power(lists:sort(Hand2))} of
        {H1, H2} when H1 > H2 -> false;
        {H1, H2} when H1 < H2 -> true;
        {H, H} -> compare(Hand1, Hand2) 
    end.

compare([], []) ->
    true;
compare([Hand1 | Rest1], [Hand2 | Rest2]) ->
    case card(Hand1, Hand2) of
        higher -> false;
        lower -> true;
        draw -> compare(Rest1, Rest2)
    end.

sort2({Hand1, _Bet1}, {Hand2, _Bet2}) ->
    case {power(lists:sort(replace_j(Hand1))), power(lists:sort(replace_j(Hand2)))} of
        {H1, H2} when H1 > H2 -> false;
        {H1, H2} when H1 < H2 -> true;
        {H, H} -> compare2(Hand1, Hand2) 
    end.

compare2([], []) ->
    true;
compare2([j | Rest1], [j | Rest2]) ->
    compare2(Rest1, Rest2);
compare2([j | _], [_ | _]) ->
    true;
compare2([_ | _], [j | _]) ->
    false;
compare2([Hand1 | Rest1], [Hand2 | Rest2]) ->
    case card(Hand1, Hand2) of
        higher -> false;
        lower -> true;
        draw -> compare2(Rest1, Rest2)
    end.

power([V, V, V, V, V]) -> 7;
power([V, V, V, V, _]) -> 6;
power([V, V, V, _, V]) -> 6;
power([V, V, _, V, V]) -> 6;
power([V, _, V, V, V]) -> 6;
power([_, V, V, V, V]) -> 6;
power([V, V, V, O, O]) -> 5;
power([O, O, V, V, V]) -> 5;
power([_, _, V, V, V]) -> 4;
power([_, V, V, V, _]) -> 4;
power([V, V, V, _, _]) -> 4;
power([V, V, O, O, _]) -> 3;
power([_, V, V, O, O]) -> 3;
power([V, V, _, O, O]) -> 3;
power([V, V, _, _, _]) -> 2;
power([_, V, V, _, _]) -> 2;
power([_, _, V, V, _]) -> 2;
power([_, _, _, V, V]) -> 2;
power([_, _, _, _, _]) -> 1.

card(A, A) -> draw;
card(A, B) when is_integer(A) andalso is_integer(B) andalso (A > B) -> higher;
card(A, B) when is_integer(A) andalso is_integer(B) andalso (A < B) -> lower;
card(A, B) when is_integer(A) andalso not is_integer(B) -> lower;
card(A, B) when not is_integer(A) andalso is_integer(B) -> higher;
card(j, q) -> lower;
card(j, k) -> lower;
card(j, a) -> lower;
card(q, j) -> higher;
card(q, k) -> lower;
card(q, a) -> lower;
card(k, j) -> higher;
card(k, q) -> higher;
card(k, a) -> lower;
card(a, j) -> higher;
card(a, q) -> higher;
card(a, k) -> higher.

replace_j(Hand) ->
    case lists:member(j, Hand) of
        true ->
            HandCounts = lists:foldl(fun
                                        (j, Counts) ->
                                            Counts;
                                        (H, Counts) ->
                                            maps:update_with(H, fun (P) -> P + 1 end, 1, Counts)
                                    end, #{}, Hand),
            MostCommon = lists:foldl(fun
                                         (Init, null) ->
                                             Init;
                                         ({H, C}, {_, Max}) when Max < C ->
                                             {H, C};
                                         (_, Agg) ->
                                             Agg
                                     end, null, maps:to_list(HandCounts)),
            ReplaceWith = case MostCommon of
                              null -> a;
                              {H, _} -> H
                          end,
            lists:map(fun (j) -> ReplaceWith; (H) -> H end, Hand);
        false ->
            Hand
    end.

input() ->
    {ok, I} = file:read_file(input),
    Lines = binary:split(I, <<"\n">>, [global]),
    lists:map(fun (R) ->
                  [HandRaw, BetRaw] = binary:split(R, <<" ">>),
                  {parse_hand(binary_to_list(HandRaw), []), binary_to_integer(BetRaw)}
              end, Lines).

parse_hand([], Hand) ->
    lists:reverse(Hand);
parse_hand([$T | Rest], Hand) ->
    parse_hand(Rest, [10 | Hand]);
parse_hand([$J | Rest], Hand) ->
    parse_hand(Rest, [j | Hand]);
parse_hand([$Q | Rest], Hand) ->
    parse_hand(Rest, [q | Hand]);
parse_hand([$K | Rest], Hand) ->
    parse_hand(Rest, [k | Hand]);
parse_hand([$A | Rest], Hand) ->
    parse_hand(Rest, [a | Hand]);
parse_hand([V | Rest], Hand) ->
    parse_hand(Rest, [list_to_integer([V]) | Hand]).
