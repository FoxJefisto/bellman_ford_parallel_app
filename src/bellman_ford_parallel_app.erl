%%%-------------------------------------------------------------------
%% @doc bellman_ford_parallel public API
%% @end
%%%-------------------------------------------------------------------

-module(bellman_ford_parallel_app).

-export([start/0]).

-import(file_utils, [read_file/1]).
-import(list_utils, [transpose/1, replace_value_in_list/3, split_list/2]).
-import(algorithm, [bellman_ford/3]).

start() ->
    {PCount, N, Matrix} = file_utils:read_file("input1.txt"),
    Matrix_T = list_utils:transpose(Matrix),
    Dist = parallel_bellman_ford(N, Matrix_T, PCount),
    io:format("Dist: ~p~n", [Dist]),
    file:write_file("output.txt", io_lib:format("~p~n", [Dist])).

parallel_bellman_ford(N, Matrix_T, PCount) ->
    ClientPid = self(),
    Vertices = list_utils:split_list(lists:seq(1, N), PCount),
    
    Start = erlang:now(),
    
    Pids = lists:map(fun(IP) ->
        spawn(fun() -> calculate_bellman_ford(ClientPid, N, Matrix_T, lists:nth(IP, Vertices)) end)
    end, lists:seq(1, PCount)),
    {Dist, MemoryUsage} = collect_results(Pids),

    Finish = erlang:now(),
    Time = timer:now_diff(Finish, Start) / 1000000,
    
    io:format("Execution time of parallel_bellman_ford: ~p seconds~n", [Time]),
    io:format("Total program processes memory usage: ~p bytes~n", [MemoryUsage]),
    io:format("Total Erlang VM memory usage: ~p bytes~n", [erlang:memory(total)]),
    Dist.


calculate_bellman_ford(ClientPid, N, Matrix_T, Vertices) ->
    ProcessPid = self(),

    ResultList = lists:map(fun(I) -> 
        Result = lists:nth(1, algorithm:bellman_ford(N, Matrix_T, I)),
        Result
    end, Vertices),

    {_, MemoryUsage} = erlang:process_info(ProcessPid, memory),
    
    io:format("[Process ~p] Process finished with result Vertices: ~p ~n", [ProcessPid, Vertices]),
    % io:format("[Process ~p] Memory usage: ~p~n", [ProcessPid, MemoryUsage]),
    ClientPid ! {ProcessPid, ResultList, MemoryUsage}.

collect_results([]) ->
    {[], 0};
collect_results([Pid|TailPids]) ->
    receive
        {Pid, ResultList, MemoryUsage} ->
            {TailList, TailMemoryUsage} = collect_results(TailPids),
            {ResultList ++ TailList, MemoryUsage + TailMemoryUsage}
    after 10 ->
        io:format("Process ~p timed out~n", [Pid]),
        collect_results(TailPids)
    end.