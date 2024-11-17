-module(file_utils).

-export([read_file/1]).

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