-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    Grid = input(),
    SortedInput = lists:sort(fun sort/2, maps:to_list(Grid)),
    AdjNrs = lists:filtermap(fun
                                 ({Pos, Value}) when is_integer(Value) ->
                                     case is_last_digit(Pos, Grid) of
                                         {true, Nr, Positions} ->
                                             case has_adjacent_symbol(Positions, Grid) of
                                                 true ->
                                                    {true, Nr};
                                                 false ->
                                                    false
                                             end;
                                         false ->
                                             false
                                     end;
                                 (_) ->
                                     false
                             end, SortedInput),
    lists:sum(AdjNrs).

part2() ->
    Grid = input(),
    Gears = lists:filtermap(fun
                                ({Pos, <<"*">>}) ->
                                    Adjacents = adjacents(Pos),
                                    {AdjDigits, _} = lists:foldl(fun (P, {Acc, Used}) ->
                                                                 V = maps:get(P, Grid, out_of_grid),
                                                                 case is_integer(V) of
                                                                    true ->
                                                                        {Nr, Using} = get_digits(P, Grid),
                                                                        Overlap = lists:any(fun (U) -> lists:member(U, Used) end, Using),
                                                                        case Overlap of
                                                                            true ->
                                                                                {Acc, Used};
                                                                            false ->
                                                                                {[Nr | Acc], Using ++ Used}
                                                                        end;
                                                                    false ->
                                                                        {Acc, Used}
                                                                 end
                                                             end, {[], []}, Adjacents),
                                    case AdjDigits of
                                        [A, B] ->
                                            {true, A * B};
                                        _ ->
                                            false
                                    end;
                                (_) ->
                                    false
                            end, maps:to_list(Grid)),
    lists:sum(Gears).

is_last_digit({X, Y}, Grid) ->
    case maps:get({X + 1, Y}, Grid, out_of_grid) of
        I when is_integer(I) ->
            false;
        _ ->
            {Nr, Positions} = get_nr_positions({X, Y}, Grid, {[], <<"">>}),
            {true, Nr, Positions}
    end.

get_nr_positions({X, Y}, Grid, {Positions, Nr}) ->
    case maps:get({X, Y}, Grid, out_of_grid) of
        I when is_integer(I) ->
            Ibinary = integer_to_binary(I),
            get_nr_positions({X - 1, Y}, Grid, {[{X, Y} | Positions], <<Ibinary/binary, Nr/binary>>});
        _ ->
            {binary_to_integer(Nr), Positions}
    end.

has_adjacent_symbol([], _) ->
    false;
has_adjacent_symbol([Pos | Rest], Grid) ->
    Adjacents = adjacents(Pos),
    Has = lists:any(fun (P) ->
                        V = maps:get(P, Grid, out_of_grid),
                        is_binary(V) andalso (V /= <<".">>)
                    end, Adjacents),
    case Has of
        true -> true;
        false -> has_adjacent_symbol(Rest, Grid)
    end.

get_digits({X, Y} = P, Grid) ->
    {Before, P1} = get_nr({X - 1, Y}, Grid, <<"">>, pre, []),
    {After, P2} = get_nr({X + 1, Y}, Grid, <<"">>, post, []),
    V = integer_to_binary(maps:get({X, Y}, Grid)),
    Nr = <<Before/binary, V/binary, After/binary>>,
    {binary_to_integer(Nr), P1 ++ P2 ++ [P]}.

get_nr({X, Y} = P, Grid, Nr, Where, Used) ->
    case maps:get({X, Y}, Grid, out_of_grid) of
        V when is_integer(V) ->
            NewNr = add_to_nr(Nr, Where, integer_to_binary(V)),
            get_nr(next({X, Y}, Where), Grid, NewNr, Where, [P | Used]);
        _ ->
            {Nr, Used}
    end.

next({X, Y}, pre) -> {X - 1, Y};
next({X, Y}, post) -> {X + 1, Y}.

add_to_nr(Nr, pre, What) -> <<What/binary, Nr/binary>>;
add_to_nr(Nr, post, What) -> <<Nr/binary, What/binary>>.

adjacents({X, Y}) ->
    [
        {X - 1, Y},
        {X - 1, Y - 1},
        {X - 1, Y + 1},
        {X + 1, Y},
        {X + 1, Y - 1},
        {X + 1, Y + 1},
        {X, Y - 1},
        {X, Y + 1}
    ].

sort({{_X1, Y1}, _}, {{_X2, Y2}, _}) when Y1 < Y2 -> true;
sort({{X1, Y}, _}, {{X2, Y}, _}) when X1 < X2 -> true;
sort(_, _) -> false.

input() ->
    {ok, I} = file:read_file(input),
    Lines = binary:split(I, <<"\n">>, [global]),
    lists:foldl(fun ({Line, Y}, G) ->
                    Es = binary_to_list(Line),
                    NewG = lists:foldl(fun ({E, X}, Gacc) ->
                                                Value = case E of
                                                            V when V >= 48 andalso V =< 57 ->
                                                                list_to_integer([V]);
                                                            V ->
                                                                list_to_binary([V]) 
                                                        end,
                                                NewGacc = maps:put({X, Y}, Value, Gacc),
                                                NewGacc
                                            end, G, lists:zip(Es, lists:seq(0, length(Es) - 1))),
                    NewG
                end, #{}, lists:zip(Lines, lists:seq(0, length(Lines) - 1))).