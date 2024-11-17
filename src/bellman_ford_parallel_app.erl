%%%-------------------------------------------------------------------
%% @doc bellman_ford_parallel public API
%% @end
%%%-------------------------------------------------------------------

-module(bellman_ford_parallel_app).

-export([start/0]).

-import(file_utils, [read_file/1]).
-import(list_utils, [transpose/1, replace_value_in_list/3]).
-import(algorithm, [bellman_ford/3]).

start() ->
    {N, Matrix} = file_utils:read_file("input1.txt"),
    Matrix_T = list_utils:transpose(Matrix),
    Start = erlang:now(),
    Dist = parallel_bellman_ford(N, Matrix_T),
    Finish = erlang:now(),
    Time = timer:now_diff(Finish, Start) / 1000000,
    io:format("Execution time of parallel_bellman_ford: ~p seconds~n", [Time]),
    io:format("Dist: ~p~n", [Dist]),
    file:write_file("output.txt", io_lib:format("~p~n", [Dist])).

parallel_bellman_ford(N, Matrix_T) ->
    ClientPid = self(),
    Pids = lists:map(fun(I) ->
        spawn(fun() -> calculate_bellman_ford(ClientPid, N, Matrix_T, I) end)
    end, lists:seq(1, N)),
    collect_results(Pids).

calculate_bellman_ford(ClientPid, N, Matrix_T, I) ->
    ProcessPid = self(),
    Result = lists:nth(1, algorithm:bellman_ford(N, Matrix_T, I)),
    io:format("Process ~p finished with result [Vertex: ~p, Minimum distance path: ~p]~n", [self(), I, Result]),
    ClientPid ! {ProcessPid, Result}.

collect_results([]) ->
    [];
collect_results([Pid|Pids]) ->
    receive
        {Pid, Result} ->
            [Result|collect_results(Pids)]
    after 10 ->
        io:format("Process ~p timed out~n", [Pid]),
        collect_results(Pids)
    end.