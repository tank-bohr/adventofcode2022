-module(day5).

-export([part1/0]).
-export([part2/0]).

-record(move, {count = 0, from, to}).

-define(REGEX, <<"move \\s (?<COUNT>\\d+) \\s from \\s (?<FROM>\\d+) \\s to \\s (?<TO>\\d+)">>).

part1() ->
    {ok, Input} = file:read_file("day5.input"),
    {Stacks, Moves} = parse(Input),
    iolist_to_binary(pop(play(Stacks, Moves, fun crate_mover_9000/2))).

part2() ->
    {ok, Input} = file:read_file("day5.input"),
    {Stacks, Moves} = parse(Input),
    iolist_to_binary(pop(play(Stacks, Moves, fun crate_mover_9001/2))).

parse(Input) ->
    [Drawing, Rearrangement] = binary:split(Input, <<"\n\n">>),
    {parse_drawing(Drawing), parse_rearrangement(Rearrangement)}.

parse_drawing(Input) ->
    parse_drawing(Input, []).

parse_drawing(Input, Acc) ->
    case binary:split(Input, <<"\n">>) of
        [Line] ->
            Indices = parse_drawing_line(Line),
            transpose(Indices, Acc);
        [Line, Rest] ->
            Row = parse_drawing_line(Line),
            parse_drawing(Rest, [Row | Acc])
    end.

parse_drawing_line(Line) ->
     <<
        S1:3/binary, " ",
        S2:3/binary, " ",
        S3:3/binary, " ",
        S4:3/binary, " ",
        S5:3/binary, " ",
        S6:3/binary, " ",
        S7:3/binary, " ",
        S8:3/binary, " ",
        S9:3/binary
    >> = Line,
    lists:map(fun parse_crate/1, [S1, S2, S3, S4, S5, S6, S7, S8, S9]).

parse_crate(<<"[", Crate:1/binary, "]">>) -> Crate;
parse_crate(<<"   ">>) -> undefined;
parse_crate(<<" ", Digit:1/binary, " ">>) -> binary_to_integer(Digit).

parse_rearrangement(Input) ->
    parse_rearrangement(Input, []).

parse_rearrangement(Input, Acc) ->
    case binary:split(Input, <<"\n">>) of
        [<<>>] ->
            lists:reverse(Acc);
        [Line, Rest] ->
            Move = parse_rearrangement_line(Line),
            parse_rearrangement(Rest, [Move | Acc])
    end.

parse_rearrangement_line(Line) ->
    {ok, Re} = re:compile(?REGEX, [extended]),
    parse_rearrangement_line(Line, Re).

parse_rearrangement_line(Line, Re) ->
    {match, [Count, From, To]} = re:run(Line, Re, [{capture, all_names, binary}]),
    #move{
        count = binary_to_integer(Count),
        from  = binary_to_integer(From),
        to    = binary_to_integer(To)
    }.

transpose(Indices, Rows) ->
    lists:foldl(fun (Row, Acc) ->
        lists:foldl(fun({Index, Crate}, Stacks) ->
            Stack = maps:get(Index, Stacks, []),
            case Crate of
                undefined -> Stacks;
                _ -> maps:put(Index, [Crate | Stack], Stacks)
            end
        end, Acc, lists:zip(Indices, Row))
    end, #{}, Rows).

pop(Stacks) ->
    lists:map(fun({_Idx, [Crate | _]}) -> Crate end, lists:usort(maps:to_list(Stacks))).

play(Stacks, Moves, Fun) ->
    lists:foldl(Fun, Stacks, Moves).

crate_mover_9000(#move{count = 0}, Stacks) ->
    Stacks;
crate_mover_9000(#move{count = Count, from = From, to = To} = Move, Stacks) ->
    crate_mover_9000(Move#move{count = Count - 1}, move(From, To, Stacks)).

crate_mover_9001(#move{count = Count, from = From, to = To}, Stacks) ->
    Source = maps:get(From, Stacks),
    Target = maps:get(To, Stacks),
    {Batch, Rest} = take_n(Count, Source),
    New = Batch ++ Target,
    maps:update(To, New, maps:update(From, Rest, Stacks)).

move(From, To, Stacks) ->
    Source = maps:get(From, Stacks),
    Target = maps:get(To, Stacks),
    {Crate, Rest} = take_from_stack(Source),
    New = put_to_stack(Crate, Target),
    maps:update(To, New, maps:update(From, Rest, Stacks)).

take_from_stack([Top | Rest]) ->
    {Top, Rest}.

put_to_stack(Crate, Stack) ->
    [Crate | Stack].

take_n(N, List) ->
    take_n(N, List, []).

take_n(0, Rest, Result) ->
    {lists:reverse(Result), Rest};
take_n(N, [Elem | Rest], Result) ->
    take_n(N - 1, Rest, [Elem | Result]).
