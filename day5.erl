-module(day5).

-export([part1/0]).

-record(movement, {count = 0, from, to}).

-define(REGEX, <<"move \\s (?<COUNT>\\d+) \\s from \\s (?<FROM>\\d+) \\s to \\s (?<TO>\\d+)">>).

part1() ->
    {ok, Input} = file:read_file("day5.input"),
    parse(Input).

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
    #movement{
        count = binary_to_integer(Count),
        from  = binary_to_integer(From),
        to    = binary_to_integer(To)
    }.

transpose(Indices, Rows) ->
    lists:foldl(fun (Row, Acc) ->
        lists:foldl(fun({Index, Crate}, Acc) ->
            Stack = maps:get(Index, Acc, []),
            case Crate of
                undefined -> Acc;
                _ -> maps:put(Index, [Crate | Stack], Acc)
            end
        end, Acc, lists:zip(Indices, Row))
    end, #{}, Rows).
