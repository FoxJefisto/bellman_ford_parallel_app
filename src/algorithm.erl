-module(algorithm).

-define(LARGE_NUMBER, 100000).

-export([bellman_ford/3]).

bellman_ford(N, Mat, Start) ->
    DistStart = list_utils:replace_value_in_list(lists:duplicate(N, ?LARGE_NUMBER), Start, 0),
    % Выполняем алгоритм Беллмана-Форда
    {ResultDist, _}= lists:foldl(fun(_, {DistCurrent, IsChangingCurrent}) -> relax(N, Mat, {DistCurrent, IsChangingCurrent}) end, {DistStart, true}, lists:seq(0, N-1)),
    ResultDist.

relax(N, Mat, {Dist, IsChanging}) ->
    if
        IsChanging ->
            DistNewAfterAllRows = lists:foldl(fun(I, DistCurrent) -> relax_vertex(N, lists:nth(I, Mat), DistCurrent, I) end, Dist, lists:seq(1, N)),
            IsChanged = Dist /= DistNewAfterAllRows,
            {DistNewAfterAllRows, IsChanged};
        true -> {Dist, IsChanging}
    end.

relax_vertex(N, Row, Dist, I) ->
    DistNewAfterRow = lists:map(fun(J) ->
        WeightEdge = lists:nth(J, Row),
        DistFrom = lists:nth(I, Dist),
        DistTo = lists:nth(J, Dist),
        if
            WeightEdge == 0 -> DistTo;
            true -> min(DistTo, DistFrom + WeightEdge)
        end
    end, lists:seq(1, N)),
    DistNewAfterRow.