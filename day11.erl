-module(day11).

-export([part1/0]).

-record(test, {
    divisible_by :: non_neg_integer(),
    if_true      :: non_neg_integer(),
    if_false     :: non_neg_integer()
}).

-record(monkey, {
    number        :: non_neg_integer(),
    items = []    :: list(non_neg_integer()),
    operation     :: function(),
    test          :: #test{},
    inspected = 0 :: non_neg_integer()
}).

part1() ->
    part1(20).

part1(Rounds) ->
    {ok, Input} = file:read_file("day11.input"),
    monkey_business(play_rounds(Rounds, to_map(parse(Input)))).

parse(Input) ->
    lists:map(fun parse_monkey_data/1,
        binary:split(Input, <<"\n\n">>, [global])).

parse_monkey_data(<<"Monkey ", Number:1/binary,":\n", Rest/binary>>) ->
    Monkey = #monkey{number = binary_to_integer(Number)},
    parse_starting_items(Rest, Monkey).

parse_starting_items(<<"  Starting items: ", Rest0/binary>>, Monkey0) ->
    [Items0, Rest] = binary:split(Rest0, <<"\n">>),
    Items = lists:map(fun binary_to_integer/1,
        binary:split(Items0, <<", ">>, [global])),
    Monkey = Monkey0#monkey{items = Items},
    parse_operation(Rest, Monkey).

parse_operation(<<"  Operation: new = ", Rest0/binary>>, Monkey0) ->
    [Line, Rest] = binary:split(Rest0, <<"\n">>),
    [Op1, Op, Op2] = binary:split(Line, <<" ">>, [global]),
    Operand1 = operand(Op1),
    Operand2 = operand(Op2),
    Operation = operation(Op),
    Monkey = Monkey0#monkey{operation = build_fun(Operation, Operand1, Operand2)},
    parse_test(Rest, Monkey).

parse_test(<<"  Test: divisible by ", Rest0/binary>>, Monkey) ->
    [DivBy, IfTrue, IfFalse | _] = binary:split(Rest0, <<"\n">>, [global]),
    Test = #test{
        divisible_by = binary_to_integer(DivBy),
        if_true      = parse_test_if_true(IfTrue),
        if_false     = parse_test_if_false(IfFalse)
    },
    Monkey#monkey{test = Test}.

parse_test_if_true(<<"    If true: throw to monkey ", Number/binary>>) -> binary_to_integer(Number).
parse_test_if_false(<<"    If false: throw to monkey ", Number/binary>>) -> binary_to_integer(Number).

operand(<<"old">>) -> old;
operand(Number) -> binary_to_integer(Number).

operation(<<"*">>) -> fun(X, Y) -> X * Y end;
operation(<<"+">>) -> fun(X, Y) -> X + Y end.

build_fun(Fun, old, old) -> fun(X) -> Fun(X, X) end;
build_fun(Fun, old, Num) -> fun(X) -> Fun(X, Num) end.

to_map(Monkeys) ->
    lists:foldl(fun(#monkey{number = Number} = Monkey, Acc) ->
        maps:put(Number, Monkey, Acc)
    end, #{}, Monkeys).

play_rounds(0, Monkeys) ->
    Monkeys;
play_rounds(Rounds, Monkeys) ->
    play_rounds(Rounds - 1, play_round(Monkeys)).

play_round(Monkeys) ->
    Keys = lists:usort(maps:keys(Monkeys)),
    play_round(Keys, Monkeys).

play_round([], Monkeys) ->
    Monkeys;
play_round([Number | Rest], Monkeys) ->
    play_round(Rest, throw_items(Number, Monkeys)).

throw_items(Number, Monkeys) ->
    Transitions = calculate_transitions(maps:get(Number, Monkeys)),
    apply_transitions(Transitions, clear_items(Number, track_inspections(Number, Monkeys))).

calculate_transitions(#monkey{items = Items} = Monkey) ->
    lists:foldl(fun(Item, Acc) -> throw_item(Item, Monkey, Acc) end, #{}, Items).

throw_item(Item, #monkey{operation = Operation, test = Test}, Acc) ->
    WorryLevel = relief(Operation(Item)),
    Target = case is_divisible_by(WorryLevel, Test#test.divisible_by) of
        true -> Test#test.if_true;
        false -> Test#test.if_false
    end,
    maps:update_with(Target, fun(Items) -> [WorryLevel | Items] end, [WorryLevel], Acc).

relief(WorryLevel) ->
    floor(WorryLevel / 3).

is_divisible_by(WorryLevel, DivBy) ->
    WorryLevel rem DivBy =:= 0.

track_inspections(Number, Monkeys) ->
    maps:update_with(Number, fun(#monkey{items = Items, inspected = Inspected} = Monkey) ->
        Monkey#monkey{inspected = Inspected + length(Items)} end,
    Monkeys).

clear_items(Number, Monkeys) ->
    maps:update_with(Number, fun(Monkey) -> Monkey#monkey{items = []} end, Monkeys).

apply_transitions(Transitions, Monkeys) ->
    maps:fold(fun(Number, Items, Acc) ->
        ItemsToAdd = lists:reverse(Items),
        maps:update_with(Number, fun(#monkey{items = Prev} = Monkey) ->
            Monkey#monkey{items = Prev ++ ItemsToAdd}
        end, Acc)
    end, Monkeys, Transitions).

monkey_business(Monkeys) ->
    Inspections = maps:fold(fun(_Number, #monkey{inspected = I}, Acc) -> [I | Acc] end, [], Monkeys),
    [A, B | _] = lists:sort(fun desc/2, Inspections),
    A * B.

desc(A, B) -> A > B.
