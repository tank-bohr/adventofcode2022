-module(day2).

-export([part1/0]).
-export([part2/0]).

part1() ->
    {ok, Data} = file:read_file("day2.input"),
    Strategy = parse(Data),
    Plays = lists:map(fun play_round/1, Strategy),
    TotalScore = lists:sum(Plays),
    TotalScore.

part2() ->
    {ok, Data} = file:read_file("day2.input"),
    Strategy = parse2(Data),
    Plays = lists:map(fun play_round/1, Strategy),
    TotalScore = lists:sum(Plays),
    TotalScore.

parse(Data) ->
    parse(Data, []).

parse(Data, Acc) ->
    case binary:split(Data, <<"\n">>) of
        [<<Opp:1/binary, " " , You:1/binary>>, Rest] ->
            Round = {parse_opp(Opp), parse_you(You)},
            parse(Rest, [Round | Acc]);
        _ -> Acc
    end.

parse2(Data) ->
    parse2(Data, []).

parse2(Data, Acc) ->
    case binary:split(Data, <<"\n">>) of
        [<<Opp:1/binary, " " , Res:1/binary>>, Rest] ->
            Opponent = parse_opp(Opp),
            Result = parse_result(Res),
            Round = {Opponent, calc_target(Result, Opponent)},
            parse2(Rest, [Round | Acc]);
        _ ->
            lists:reverse(Acc)
    end.

parse_opp(<<"A">>) -> rock;
parse_opp(<<"B">>) -> paper;
parse_opp(<<"C">>) -> scissors.

parse_you(<<"X">>) -> rock;
parse_you(<<"Y">>) -> paper;
parse_you(<<"Z">>) -> scissors.

parse_result(<<"X">>) -> lose;
parse_result(<<"Y">>) -> draw;
parse_result(<<"Z">>) -> win.

calc_target(win, rock) -> paper;
calc_target(win, paper) -> scissors;
calc_target(win, scissors) -> rock;
calc_target(lose, rock) -> scissors;
calc_target(lose, paper) -> rock;
calc_target(lose, scissors) -> paper;
calc_target(draw, Result) -> Result.

play_round({Opp, You}) ->
    score(You) + outcome(Opp, You).

score(rock) -> 1;
score(paper) -> 2;
score(scissors) -> 3.

outcome(rock, paper) -> 6;
outcome(paper, scissors) -> 6;
outcome(scissors, rock) -> 6;
outcome(Opp, You) when Opp =:= You -> 3;
outcome(_, _) -> 0.
