-module(day9).

-export([part1/0]).

-record(motion, {
    direction :: right | up | left | down,
    steps     :: non_neg_integer()
}).

-record(state, {
    head = {0, 0},
    tail = {0, 0}
}).

part1() ->
    {ok, Input} = file:read_file("day9.input"),
    Movements = parse(Input),
    States = play(Movements),
    % States.
    Tails = lists:map(fun(#state{tail = Tail}) -> Tail end, States),
    % lists:usort(Tails).
    length(lists:uniq(Tails)).

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

parse_line(<<Direction:1/binary, " ", Steps/binary>>) ->
    #motion{
        direction = direction(Direction),
        steps     = binary_to_integer(Steps)
    }.

direction(<<"R">>) -> right;
direction(<<"L">>) -> left;
direction(<<"D">>) -> down;
direction(<<"U">>) -> up.

play(Motions) ->
    play(Motions, [#state{}]).

play([], Acc) ->
    lists:reverse(Acc);
play([Motion | Motions], Acc) ->
    play(Motions, run_motion(Motion, Acc)).

run_motion(#motion{steps = 0}, Acc) ->
    Acc;
run_motion(#motion{direction = Direction, steps = Steps} = Motion, [State | _] = Acc) ->
    Head = move_head(Direction, State#state.head),
    Tail = move_next(Head, State#state.tail),
    run_motion(Motion#motion{steps = Steps - 1}, [State#state{head = Head, tail = Tail} | Acc]).

move_head(right, {X, Y}) -> {X + 1, Y};
move_head(left, {X, Y}) -> {X - 1, Y};
move_head(down, {X, Y}) -> {X, Y - 1};
move_head(up, {X, Y}) -> {X, Y + 1}.

move_next({HX, Y}, {TX, Y}) when abs(HX - TX) > 1, HX > TX -> {TX + 1, Y};
move_next({HX, Y}, {TX, Y}) when abs(HX - TX) > 1, HX < TX -> {TX - 1, Y};
move_next({X, HY}, {X, TY}) when abs(HY - TY) > 1, HY > TY -> {X, TY + 1};
move_next({X, HY}, {X, TY}) when abs(HY - TY) > 1, HY < TY -> {X, TY - 1};
move_next({HX, HY}, {TX, TY} = Tail) when abs(HX - TX) =< 1, abs(HY - TY) =< 1 -> Tail;
move_next({HX, HY}, {TX, TY}) when HX > TX, HY > TY -> {TX + 1, TY + 1};
move_next({HX, HY}, {TX, TY}) when HX > TX, HY < TY -> {TX + 1, TY - 1};
move_next({HX, HY}, {TX, TY}) when HX < TX, HY > TY -> {TX - 1, TY + 1};
move_next({HX, HY}, {TX, TY}) when HX < TX, HY < TY -> {TX - 1, TY - 1}.
