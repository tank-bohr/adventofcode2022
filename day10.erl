-module(day10).

-export([part1/0]).
-export([part2/0]).

-record(entry, {
    cmd       :: noop | addx,
    args = [] :: list(integer())
}).

-record(state, {
    x     = 1 :: integer(),
    cycle = 1 :: non_neg_integer()
}).

-define(STRENGTH_CYCLES, [20, 60, 100, 140, 180, 220]).
-define(WIDTH, 40).
-define(LIT, $#).
-define(DARK, $.).

part1() ->
    {ok, Input} = file:read_file("day10.input"),
    lists:sum(signal_strengths(execute(parse(Input)))).

part2() ->
    {ok, Input} = file:read_file("day10.input"),
    lists:foreach(fun(Line) -> io:format("~s~n", [Line]) end,
        lists:map(fun draw_row/1, cut(execute(parse(Input))))).

parse(Input) ->
    parse(Input, []).

parse(Input, Acc) ->
    case binary:split(Input, <<"\n">>) of
        [Line, Rest] ->
            Command = parse_line(Line),
            parse(Rest, [Command | Acc]);
        [<<>>] ->
            lists:reverse(Acc)
    end.

parse_line(<<"addx ", Arg/binary>>) ->
    #entry{cmd = addx, args = [binary_to_integer(Arg)]};
parse_line(<<"noop">>) ->
    #entry{cmd = noop}.

execute(Commands) ->
    execute(Commands, [#state{}]).

execute([], Acc) ->
    lists:reverse(Acc);
execute([Command | Rest], [State | _] = Acc) ->
    Result = execute_command(Command, State),
    execute(Rest, Result ++ Acc).

execute_command(#entry{cmd = noop}, State) ->
    [inc_cycle(State)];
execute_command(#entry{cmd = addx, args = [Arg]}, State) ->
    State1 = inc_cycle(State),
    State2 = do_addx(State1, Arg),
    [State2, State1].

inc_cycle(#state{cycle = Cycle} = State) ->
    State#state{cycle = Cycle + 1}.

do_addx(#state{cycle = Cycle, x = X} = State, Arg) ->
    State#state{cycle = Cycle + 1, x = X + Arg}.

signal_strengths(States) ->
    signal_strengths(States, ?STRENGTH_CYCLES, []).

signal_strengths([], _Cycles, Acc) ->
    Acc;
signal_strengths(_States, [], Acc) ->
    Acc;
signal_strengths([#state{cycle = Cycle, x = X} | Rest], [Cycle | Cycles], Acc) ->
    SignalStrength = X * Cycle,
    signal_strengths(Rest, Cycles, [SignalStrength | Acc]);
signal_strengths([_State | Rest], Cycles, Acc) ->
    signal_strengths(Rest, Cycles, Acc).

draw_row(States) ->
    draw_row(States, 0, []).

draw_row([], _Position, Acc) ->
    lists:reverse(Acc);
draw_row([#state{x = X} | Rest], Position, Acc) ->
    case does_cover(X, Position) of
        true ->
            draw_row(Rest, Position + 1, [?LIT | Acc]);
        false ->
            draw_row(Rest, Position + 1, [?DARK | Acc])
    end.

does_cover(X, Position) ->
    (X - 1 =< Position) and (Position =< X + 1).

cut(States) ->
    cut(States, 1, []).

cut(States, Start, Acc) when Start > length(States) ->
    lists:reverse(Acc);
cut(States, Start, Acc) ->
    Row = lists:sublist(States, Start, ?WIDTH),
    cut(States, Start + ?WIDTH, [Row | Acc]).
