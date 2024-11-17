-module(list_utils).

-export([transpose/1, replace_value_in_list/3]).

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