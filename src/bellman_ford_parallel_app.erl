%%%-------------------------------------------------------------------
%% @doc bellman_ford_parallel public API
%% @end
%%%-------------------------------------------------------------------

-module(bellman_ford_parallel_app).

-export([start/0]).

-define(LARGE_NUMBER, 100000).

start() ->
    {N, Matrix} = read_file("input1.txt"),
    Dist = bellman_ford(N, Matrix),
    file:write_file("output.txt", io_lib:format("~p~n", [Dist])).

read_file(Filename) ->
    {ok, Fd} = file:open(Filename, [read]),
    {ok, [N]} = io:fread(Fd, [], "~d"),
    Matrix = read_lines(Fd),
    file:close(Fd),
    {N, Matrix}.


read_lines(Fd) ->
    read_lines(Fd, []).

read_lines(Fd, Acc) ->
    case io:get_line(Fd, "") of
        eof ->
            lists:reverse(Acc);
        {error, Reason} ->
            {error, Reason};
        Line ->
            Row_with_n = string:tokens(Line, " "),
            Row = [string:trim(X) || X <- Row_with_n],
            read_lines(Fd, [lists:map(fun(X) -> list_to_integer(X) end, Row)] ++ Acc)
    end.






bellman_ford(N, Mat) ->
    DistStart = [0|lists:duplicate(N-1, ?LARGE_NUMBER)],

    % Выполняем алгоритм Беллмана-Форда
    lists:foldl(fun(_, DistCurrent) -> relax(N, Mat, DistCurrent) end, DistStart, lists:seq(0, N-1)).

relax(N, Mat, Dist) ->
    DistNewAfterAllRows = lists:foldl(fun(I, DistCurrent) -> relax_vertex(N, lists:nth(I, Mat), DistCurrent, I) end, Dist, lists:seq(1, N)),
    DistNewAfterAllRows.

relax_vertex(N, Row, Dist, I) ->
    DistNewAfterRow = lists:map(fun(J) ->
        WeightEdge = lists:nth(J, Row),
        WeightSourceVertex = lists:nth(I, Dist),
        WeightTargetVertex = lists:nth(J, Dist),
        if
            WeightEdge == 0 -> WeightTargetVertex;
            true -> min(WeightSourceVertex + WeightEdge, WeightTargetVertex)
        end
    end, lists:seq(1, N)),
    DistNewAfterRow.