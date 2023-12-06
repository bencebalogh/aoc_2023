-module(aoc).
-export([part1/0]).
-export([part2/0]).

part1() ->
    [Seeds, Rules] = input(),
    min_seed_location(Seeds, Rules).
    
part2() ->
    [SeedRanges, Rules] = input(),
    Ranges = convert_seed_ranges_to_ranges(SeedRanges, []),
    Rules2 = lists:map(fun (R) -> lists:map(fun ({Dst, Src, Len}) -> {Src, Src + Len - 1, Src - Dst} end, R) end, Rules),
    lists:min(lists:filtermap(fun ({0, _}) -> false; ({A, _}) -> {true, A} end, map(Ranges, Rules2))).

min_seed_location(Seeds, Rules) ->
    Locations = lists:foldl(fun (Rule, Locations) ->
                        lists:map(fun (Location) ->
                                      find_location(Location, Rule)
                                  end, Locations)
                    end, Seeds, Rules),
    lists:min(Locations).

find_location(Location, []) ->
    Location;
find_location(Location, [{Dst, Src, Len} | _Rest]) when (Src =< Location) andalso (Src + Len >= Location) ->
    Dst + abs(Location - Src);
find_location(Location, [_ | Rest]) ->
    find_location(Location, Rest).

map(Ranges, []) ->
    Ranges;
map(Ranges, [Mapping | Rest]) ->
    Mapped = lists:flatmap(fun (Range) -> split_range(Range, Mapping) end, Ranges),
    map(Mapped, Rest).

split_range(Range, []) ->
    [Range];
split_range({Start, Stop}, [{SrcStart, SrcStop, Diff} | _Rest]) when (Start >= SrcStart) and (Stop =< SrcStop) ->
    [{Start - Diff, Stop - Diff}];
split_range({Start, Stop}, [{SrcStart, SrcStop, Diff} | _Rest]) when (Start < SrcStart) and ((Stop >= SrcStart) and (Stop =< SrcStop)) ->
    [{Start, SrcStart - 1}, {SrcStart - Diff, Stop - Diff}];
split_range({Start, Stop}, [{SrcStart, SrcStop, Diff} | Rest]) when (Start >= SrcStart) and (Start =< SrcStop) ->
    [{Start - Diff, SrcStop - Diff} | split_range({SrcStop + 1, Stop}, Rest)];
split_range(Range, [_ | Rest]) ->
    split_range(Range, Rest).

convert_seed_ranges_to_ranges([], Ranges) ->
    Ranges;
convert_seed_ranges_to_ranges([A, B | Rest], Ranges) ->
    convert_seed_ranges_to_ranges(Rest, [{A, A + B - 1} | Ranges]).

input() ->
    {ok, I} = file:read_file(input),
    [<<"seeds: ", SeedsRaw/binary>> | Rules] = binary:split(I, <<"\n\n">>, [global]),
    Seeds = lists:map(fun (Int) -> binary_to_integer(Int) end, binary:split(SeedsRaw, <<" ">>, [global])),
    Ds = lists:map(fun (Rule) ->
                       [_, Rs] = binary:split(Rule, <<":\n">>),
                       lists:map(fun (R) ->
                                      [DstRangeStart, SrcRangeStart, Length] = binary:split(R, <<" ">>, [global]),
                                      {
                                          binary_to_integer(DstRangeStart),
                                          binary_to_integer(SrcRangeStart),
                                          binary_to_integer(Length)
                                      }
                                 end, binary:split(Rs, <<"\n">>, [global]))
                   end, Rules),
    [Seeds, Ds].