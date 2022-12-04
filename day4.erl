-module(day4).

-export([part1/0]).
-export([part2/0]).

part1() ->
    {ok, Input} = file:read_file("day4.input"),
    Data = lists:map(fun parse_pair/1, parse(Input)),
    Result = lists:filter(fun does_fully_contain/1, Data),
    length(Result).

part2() ->
    {ok, Input} = file:read_file("day4.input"),
    Data = lists:map(fun parse_pair/1, parse(Input)),
    Result = lists:filter(fun has_overlap/1, Data),
    length(Result).

parse(Input) ->
    parse(Input, []).

parse(Input, Acc) ->
    case binary:split(Input, <<"\n">>) of
        [Line, Rest] ->
            parse(Rest, [Line | Acc]);
        _ ->
            lists:reverse(Acc)
    end.

parse_pair(Pair) ->
    [First, Second] = binary:split(Pair, <<",">>),
    {parse_range(First), parse_range(Second)}.

parse_range(Range) ->
    [From, To] = binary:split(Range, <<"-">>),
    {binary_to_integer(From), binary_to_integer(To)}.

does_fully_contain({{From1, To1}, {From2, To2}}) when From1 >= From2 andalso To1 =< To2 -> true;
does_fully_contain({{From1, To1}, {From2, To2}}) when From2 >= From1 andalso To2 =< To1 -> true;
does_fully_contain(_) -> false.

has_overlap({{From1, To1}, {From2, _To2}}) when From1 =< From2 andalso From2 =< To1 -> true;
has_overlap({{From1, To1}, {_From2, To2}}) when From1 =< To2 andalso To2 =< To1 -> true;
has_overlap(Pair) -> does_fully_contain(Pair).
