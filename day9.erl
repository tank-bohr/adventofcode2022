-module(day9).

-export([part1/0]).
-export([part2/0]).

-record(motion, {
    direction :: right | up | left | down,
    steps     :: non_neg_integer()
}).

part1() -> run(2).
part2() -> run(10).

run(RopeLength) ->
    {ok, Input} = file:read_file("day9.input"),
    Movements = parse(Input),
    Rope = init_rope(RopeLength),
    Ropes = play(Movements, [Rope]),
    Tails = lists:map(fun lists:last/1, Ropes),
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

play([], Ropes) ->
    lists:reverse(Ropes);
play([Motion | Motions], Ropes) ->
    play(Motions, run_motion(Motion, Ropes)).

run_motion(#motion{steps = 0}, Ropes) ->
    Ropes;
run_motion(#motion{direction = Direction, steps = Steps} = Motion, [[Head | Rest] | _] = Ropes) ->
    NewRope = lists:reverse(lists:foldl(fun(Knot, [Prev | _] = Acc) ->
        [move_knot(Prev, Knot) | Acc]
    end, [move_head(Direction, Head)], Rest)),
    run_motion(Motion#motion{steps = Steps - 1}, [NewRope | Ropes]).

move_head(right, {X, Y}) -> {X + 1, Y};
move_head(left, {X, Y}) -> {X - 1, Y};
move_head(down, {X, Y}) -> {X, Y - 1};
move_head(up, {X, Y}) -> {X, Y + 1}.

move_knot({HX, Y}, {TX, Y}) when abs(HX - TX) > 1, HX > TX -> {TX + 1, Y};
move_knot({HX, Y}, {TX, Y}) when abs(HX - TX) > 1, HX < TX -> {TX - 1, Y};
move_knot({X, HY}, {X, TY}) when abs(HY - TY) > 1, HY > TY -> {X, TY + 1};
move_knot({X, HY}, {X, TY}) when abs(HY - TY) > 1, HY < TY -> {X, TY - 1};
move_knot({HX, HY}, {TX, TY} = Tail) when abs(HX - TX) =< 1, abs(HY - TY) =< 1 -> Tail;
move_knot({HX, HY}, {TX, TY}) when HX > TX, HY > TY -> {TX + 1, TY + 1};
move_knot({HX, HY}, {TX, TY}) when HX > TX, HY < TY -> {TX + 1, TY - 1};
move_knot({HX, HY}, {TX, TY}) when HX < TX, HY > TY -> {TX - 1, TY + 1};
move_knot({HX, HY}, {TX, TY}) when HX < TX, HY < TY -> {TX - 1, TY - 1}.

init_rope(Length) ->
    init_rope(Length, []).

init_rope(0, Rope) ->
    Rope;
init_rope(Length, Rope) ->
    init_rope(Length - 1, [{0, 0} | Rope]).
