-module(day8).

-export([part1/0]).
-export([part2/0]).

-define(EDGE, 98).

part1() ->
    {ok, Input} = file:read_file("day8.input"),
    Grid = build_grid(parse(Input)),
    lists:foldl(fun(Tree, Acc) ->
        case is_visible(Tree, Grid) of
            true -> Acc + 1;
            false -> Acc
        end
    end, 0, all()).

part2() ->
    {ok, Input} = file:read_file("day8.input"),
    Grid = build_grid(parse(Input)),
    lists:max(lists:map(fun(Tree) -> scenic_score(Tree, Grid) end, all())).

parse(Input) ->
    parse(Input, []).

parse(Input, Acc) ->
    case binary:split(Input, <<"\n">>) of
        [Line, Rest] ->
            Digits = lists:map(fun char_to_digit/1, binary_to_list(Line)),
            parse(Rest, [Digits | Acc]);
        [<<>>] ->
            lists:reverse(Acc)
    end.

char_to_digit(Char) -> Char - $0.

is_visible(Tree, Grid) ->
    Value = get_elem(Tree, Grid),
    lists:any(fun(Direction) ->
        lists:all(fun(Other) ->
            Value > get_elem(Other, Grid)
        end, apply(Direction, [Tree]))
    end, directions()).

scenic_score(Tree, Grid) ->
    Value = get_elem(Tree, Grid),
    lists:foldl(fun(Direction, Product) ->
        Trees = apply(Direction, [Tree]),
        Product * count_visible_trees(Value, Trees, Grid)
    end, 1, directions()).

count_visible_trees(Cutoff, Trees, Grid) ->
    count_visible_trees(Cutoff, Trees, Grid, 0).

count_visible_trees(_Cutoff, [], _Grid, Acc) ->
    Acc;
count_visible_trees(Cutoff, [Other | Rest], Grid, Acc) ->
    case get_elem(Other, Grid) of
        Value when Value >= Cutoff -> Acc + 1;
        Value when Value < Cutoff -> count_visible_trees(Cutoff, Rest, Grid, Acc + 1)
    end.

get_elem({RowIdx, ColIdx}, Grid) ->
    case array:get(RowIdx, Grid) of
        undefined -> -1;
        Row ->
            case array:get(ColIdx, Row) of
                undefined -> -1;
                Elem -> Elem
            end
    end.

build_grid(Rows) -> array:from_list(lists:map(fun array:from_list/1, Rows)).
all() -> [{X, Y} || X <- lists:seq(0, ?EDGE), Y <- lists:seq(0, ?EDGE)].
up({X0, Y0}) -> lists:reverse([{X0, Y} || Y <- lists:seq(0, Y0 - 1), Y >= 0]).
down({X0, Y0}) -> [{X0, Y} || Y <- lists:seq(Y0 + 1, ?EDGE), Y =< ?EDGE].
left({X0, Y0}) -> lists:reverse([{X, Y0} || X <- lists:seq(0, X0 - 1), X >= 0]).
right({X0, Y0}) -> [{X, Y0} || X <- lists:seq(X0 + 1, ?EDGE), X =< ?EDGE].
directions() -> [fun up/1, fun down/1, fun left/1, fun right/1].
