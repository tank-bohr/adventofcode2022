-module(day12).

-export([part1/0]).

-define(START, {21, 1}).
-define(END, {21, 47}).

part1() ->
    {ok, Input} = file:read_file("day12.input"),
    to_map(parse(Input)).

parse(Input) ->
    lists:map(fun binary_to_list/1,  binary:split(Input, <<"\n">>, [global])).

to_map(Rows) ->
    Length = length(Rows),
    lists:foldl(fun({Row, RowIdx}, Acc) ->
        RowLength = length(Row),
        RowMap = lists:foldl(fun({Item, ColIdx}, AccInn) ->
            Height = case Item of $S -> $a; $E -> $z; H -> H end,
            maps:put(ColIdx, Height, AccInn)
        end, #{}, lists:zip(Row, lists:seq(1, RowLength))),
        maps:put(RowIdx, RowMap, Acc)
    end, #{}, lists:zip(Rows, lists:seq(1, Length))).

neighbors({X, Y}) ->
    [
        {X + 1, Y},
        {X - 1, Y},
        {X, Y + 1},
        {X, Y - 1}
    ].

get_height({X, Y}, HeightMap) ->
    Row = maps:get(Y, HeightMap, #{}),
    maps:get(X, Row, undefined).

directions(Sq, HeightMap) ->
    MaxHeight = get_height(Neighbor, HeightMap) + 1
    lists:filter(fun(Neighbor) ->
        case get_height(Neighbor, HeightMap) of
            undefined -> false;
            Height when Height > MaxHeight -> false;
            Height when Height =< MaxHeight -> true
        end
    end, neighbors(Sq)).
