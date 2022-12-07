-module(day7).

-export([part1/0]).
-export([part2/0]).

-record(entry, {
    command     :: binary(),
    result = [] :: [binary()]
}).

-record(node, {
    parts = []     :: [binary()],
    path           :: binary(),
    size = 0       :: non_neg_integer(),
    is_dir = false :: boolean()
}).

-define(TOTAL, 70000000).
-define(REQUIERED, 30000000).

part1() ->
    {ok, Input} = file:read_file("day7.input"),
    lists:foldl(fun add_small_dir_size/2, 0,
        calulate_dir_sizes(play(parse(Input)))).

part2() ->
    {ok, Input} = file:read_file("day7.input"),
    {Dirs, Occupied} = calulate_sizes(play(parse(Input))),
    Available = ?TOTAL - Occupied,
    NeedToFree = ?REQUIERED - Available,
    {value, #node{size = Size}} = pick_dir(Dirs, NeedToFree),
    Size.

parse(Input) ->
    parse(Input, []).

parse(Input, Acc) ->
    case binary:split(Input, <<"\n">>) of
        [Line, Rest] ->
            parse(Rest, parse_line(Line, Acc));
        [<<>>] ->
            lists:reverse(Acc)
    end.

parse_line(<<"$ ", Rest/binary>>, Acc) ->
    [#entry{command = Rest} | Acc];
parse_line(ResultLine, [#entry{result = Result} = Entry | Acc]) ->
    [Entry#entry{result = [ResultLine | Result]} | Acc].

play(Entries) ->
    {_Dir, Nodes} = lists:foldl(fun run/2, {[], []}, Entries),
    Nodes.

run(#entry{command = <<"ls">>, result = Result}, {CurrentDir, Acc}) ->
    process_list(Result, CurrentDir, Acc);
run(#entry{command = <<"cd ..">>}, {[_ | CurrentDir], Acc}) ->
    {CurrentDir, Acc};
run(#entry{command = <<"cd ", Path/binary>>}, {CurrentDir, Acc}) ->
    {[Path | CurrentDir], Acc}.

process_list(Result, CurrentDir, Acc) ->
    {CurrentDir, lists:map(fun(Line) -> build_node(Line, CurrentDir) end, Result) ++ Acc}.

build_node(Line, CurrentDir) ->
    [Size, Name] = binary:split(Line, <<" ">>),
    Parts = lists:reverse([Name | CurrentDir]),
    Path = filename:join(Parts),
    case Size of
        <<"dir">> ->
            #node{parts = Parts, path = Path, is_dir = true};
        Number ->
            #node{parts = Parts, path = Path, size = binary_to_integer(Number)}
    end.

calulate_sizes(Nodes) ->
    {DirsWithoutSizes, Files} = lists:partition(fun(#node{is_dir = IsDir}) -> IsDir end, Nodes),
    DirsWithSizes = lists:map(fun(Dir) -> calulate_dir_size(Dir, Files) end, DirsWithoutSizes),
    TotalSize = lists:foldl(fun(#node{size = Size}, Acc) -> Size + Acc end, 0, Files),
    {DirsWithSizes, TotalSize}.

calulate_dir_sizes(Nodes) ->
    {Dirs, Files} = lists:partition(fun(#node{is_dir = IsDir}) -> IsDir end, Nodes),
    lists:map(fun(Dir) -> calulate_dir_size(Dir, Files) end, Dirs).

calulate_dir_size(Dir, AllFiles) ->
    Files = lists:filter(fun(Node) -> does_node_belong_to_dir(Dir, Node) end, AllFiles),
    lists:foldl(fun add_file_size_to_dir/2, Dir, Files).

does_node_belong_to_dir(#node{parts = DirParts}, #node{parts = FileParts}) ->
    lists:prefix(DirParts, FileParts).

add_file_size_to_dir(#node{size = Size}, #node{size = PrevSize} = Dir) ->
    Dir#node{size = PrevSize + Size}.

add_small_dir_size(#node{size = Size}, Acc) when Size =< 100000 -> Size + Acc;
add_small_dir_size(#node{}, Acc) -> Acc.

pick_dir(Dirs, Cutoff) ->
    lists:search(fun(#node{size = Size}) -> Size > Cutoff end,
        lists:sort(fun sort_dirs/2, Dirs)).

sort_dirs(#node{size = A}, #node{size = B}) -> A =< B.
