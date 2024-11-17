-module(list_utils).

-export([transpose/1, replace_value_in_list/3, split_list/2]).

transpose(L) ->
    transpose_do([], L).
transpose_do(Acc, [[]|_]) ->
    lists:reverse(Acc);
transpose_do(Acc, M) ->
    Row = lists:foldr(
        fun(Elem, FoldAcc) ->
                    [hd(Elem) | FoldAcc]
        end,
        [],
        M),
    transpose_do([Row|Acc], lists:map(fun(X) -> tl(X) end, M)).

replace_value_in_list(List, I, Value) ->
    ListNew = lists:map(fun(K) ->
        if 
            K == I -> Value; 
            true -> lists:nth(K, List) 
        end
    end, lists:seq(1, length(List))),
    ListNew.

split_list(List, N) ->
    Len = length(List),
    if 
        N > Len -> N1 = Len;
        true -> N1 = N
    end,
    ChunkSize = Len div N1,
    Remainder = Len rem N1,
    split_list(List, N1, ChunkSize, Remainder, []).

split_list([], _, _, _, Acc) ->
    lists:reverse(Acc);
split_list(List, N, ChunkSize, Remainder, Acc) ->
    Chunk = lists:sublist(List, ChunkSize),
    Rest = lists:nthtail(ChunkSize, List),
    Acc1 = [Chunk|Acc],
    if
        Remainder > 0 ->
            split_list(Rest, N - 1, length(Rest) div (N - 1), length(Rest) rem (N - 1), Acc1);
        true ->
            split_list(Rest, N - 1, ChunkSize, Remainder, Acc1)
    end.