-module(day3).

-export([part1/0]).
-export([part2/0]).

part1() ->
    {ok, Input} = file:read_file("day3.input"),
    Priorities = priorities(),
    lists:foldl(fun(Backpack, Sum) ->
        {value, CommonType} = find_backpack_common_type(Backpack),
        Sum + priority(CommonType, Priorities)
    end, 0, parse(Input)).

part2() ->
    {ok, Input} = file:read_file("day3.input"),
    Priorities = priorities(),
    lists:foldl(fun(Group, Sum) ->
        {value, CommonType} = find_group_common_type(Group),
        Sum + priority(CommonType, Priorities)
    end, 0, group_backpacks(parse(Input))).

parse(Input) ->
    parse(Input, []).

parse(Input, Acc) ->
    case binary:split(Input, <<"\n">>) of
        [Line, Rest] ->
            parse(Rest, [Line | Acc]);
        _ ->
            Acc
    end.

priorities() ->
    Types = lists:seq($a, $z) ++ lists:seq($A, $Z),
    Priorities = lists:seq(1, 52),
    maps:from_list(lists:zip(Types, Priorities)).

find_backpack_common_type(Backpack) ->
    {First, Second} = split_compartments(Backpack),
    find_common(binary_to_list(First), binary_to_list(Second)).

find_group_common_type({Backpack1, Backpack2, Backpack3}) ->
    Common = all_common(binary_to_list(Backpack1), binary_to_list(Backpack2)),
    find_common(Common, binary_to_list(Backpack3)).

group_backpacks(Data) ->
    group_backpacks(Data, []).

group_backpacks([], Acc) ->
    Acc;
group_backpacks([Backpack1, Backpack2, Backpack3 | Rest], Acc) ->
    group_backpacks(Rest, [{Backpack1, Backpack2, Backpack3} | Acc]).

find_common(List1, List2) ->
    lists:search(fun(Item) -> does_contain(List2, Item) end, List1).

all_common(List1, List2) ->
    lists:filter(fun(Item) -> does_contain(List2, Item) end, List1).

split_compartments(Backpack) ->
    Size = size(Backpack),
    HalfSize = Size div 2,
    First = binary:part(Backpack, {0, HalfSize}),
    Second = binary:part(Backpack, {HalfSize, HalfSize}),
    {First, Second}.

does_contain(List, Target) ->
    case lists:search(fun(Item) -> Item =:= Target end, List) of
        {value, Target} ->
            true;
        false ->
            false
    end.

priority(Type, Priorities) ->
    maps:get(Type, Priorities).
