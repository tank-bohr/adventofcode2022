-module(day6).

-export([part1/0]).
-export([part2/0]).

part1() ->
  {ok, Input} = file:read_file("day6.input"),
  find_start_of_packet_marker(Input).

part2() ->
  {ok, Input} = file:read_file("day6.input"),
  find_start_of_message_marker(Input).

find_start_of_packet_marker(Input) ->
    find_marker(Input, 4).

find_start_of_message_marker(Input) ->
    find_marker(Input, 14).

find_marker(Input, Len) ->
    <<Candidate:Len/binary, Rest/binary>> = Input,
    find_marker(Rest, Candidate, Len).

find_marker(Input, Candidate, Index) ->
    case is_marker(Candidate) of
        true ->
            Index;
        false ->
            <<_Head:1/binary, Tail/binary>> = Candidate,
            <<Char:1/binary, Rest/binary>> = Input,
            find_marker(Rest, <<Tail/binary, Char/binary>>, Index + 1)
    end.

is_marker(Bin) ->
    List = binary_to_list(Bin),
    Length = length(List),
    length(lists:uniq(List)) =:= Length.
