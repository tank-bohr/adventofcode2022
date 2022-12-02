-module(day1).

-export([
    part1/0,
    part2/0
]).

part1() ->
    {ok, Data} = file:read_file("day1.input"),
    lists:max(lists:map(fun lists:sum/1, parse(Data))).

part2() ->
    {ok, Data} = file:read_file("day1.input"),
    Inventory = parse(Data),
    Calories = lists:map(fun lists:sum/1, Inventory),
    case lists:reverse(lists:usort(Calories)) of
        [First, Second, Third | _Rest] ->
            Sum = First + Second + Third,
            {ok, Sum};
        _Else ->
            {error, not_enough_elves}
    end.


parse(Data) ->
    parse(Data, [], []).

parse(<<>>, Current, Acc) ->
    [Current | Acc];
parse(Data, Current, Acc) ->
    case binary:split(Data, <<"\n">>) of
        [<<>>, Rest] ->
            parse(Rest, [], [Current | Acc]);
        [Bin, Rest] ->
            Number = binary_to_integer(Bin),
            parse(Rest, [Number | Current], Acc)
    end.
