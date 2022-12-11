-module(day11_2).

-export([part2/0]).

-record(test, {
    divisible_by :: non_neg_integer(),
    if_true      :: non_neg_integer(),
    if_false     :: non_neg_integer()
}).

-record(monkey, {
    number              :: non_neg_integer(),
    starting_items = [] :: list(non_neg_integer()),
    items = []          :: list(map()),
    operation           :: function(),
    test                :: #test{},
    inspected = 0       :: non_neg_integer()
}).

-define(MONKEYS, [
    #monkey{
        number = 0,
        starting_items = [61],
        operation = fun(X) -> X * 11 end,
        test = #test{
            divisible_by = 5,
            if_true = 7,
            if_false = 4
        }
    },
    #monkey{
        number = 1,
        starting_items = [76, 92, 53, 93, 79, 86, 81],
        operation = fun(X) -> X + 4 end,
        test = #test{
            divisible_by = 2,
            if_true = 2,
            if_false = 6
        }
    },
    #monkey{
        number = 2,
        starting_items = [91, 99],
        operation = fun(X) -> X * 19 end,
        test = #test{
            divisible_by = 13,
            if_true = 5,
            if_false = 0
        }
    },
    #monkey{
        number = 3,starting_items = [58, 67, 66],
        operation = fun(X) -> X * X end,
        test = #test{
            divisible_by = 7,
            if_true = 6,
            if_false = 1
        }
    },
    #monkey{
        number = 4,
        starting_items = [94, 54, 62, 73],
        operation = fun(X) -> X + 1 end,
        test = #test{
            divisible_by = 19,
            if_true = 3,
            if_false = 7
        }
    },
    #monkey{
        number = 5,
        starting_items = [59, 95, 51, 58, 58],
        operation = fun(X) -> X + 3 end,
        test = #test{
            divisible_by = 11,
            if_true = 0,
            if_false = 4
        }
    },
    #monkey{
        number = 6,
        starting_items = [87, 69, 92, 56, 91, 93, 88, 73],
        operation = fun(X) -> X + 8 end,
        test = #test{
            divisible_by = 3,
            if_true = 5,
            if_false = 2
        }
    },
    #monkey{
        number = 7,
        starting_items = [71, 57, 86, 67, 96, 95],
        operation = fun(X) -> X + 7 end,
        test = #test{
            divisible_by = 17,
            if_true = 3,
            if_false = 1
        }
    }
]).

part2() ->
    monkey_business(play_rounds(10000, init_monkeys())).

init_monkeys() ->
    to_map(lists:map(fun init_monkey/1, ?MONKEYS)).

init_monkey(#monkey{starting_items = StartingItems} = Monkey) ->
    Monkey#monkey{items = lists:map(fun calc_reminders/1, StartingItems)}.

calc_reminders(Num) ->
    #{
        2  => Num rem 2,
        3  => Num rem 3,
        5  => Num rem 5,
        7  => Num rem 7,
        11 => Num rem 11,
        13 => Num rem 13,
        17 => Num rem 17,
        19 => Num rem 19
    }.

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
    WorryLevel = apply_operation(Operation, Item),
    Target = case is_divisible_by(WorryLevel, Test#test.divisible_by) of
        true -> Test#test.if_true;
        false -> Test#test.if_false
    end,
    maps:update_with(Target, fun(Items) -> [WorryLevel | Items] end, [WorryLevel], Acc).

apply_operation(Operation, Item) ->
    maps:map(fun(D, R) -> Operation(R) rem D end, Item).

is_divisible_by(WorryLevel, DivBy) ->
    maps:get(DivBy, WorryLevel) =:= 0.

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
