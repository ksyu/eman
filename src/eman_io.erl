-module(eman_io).

-export([print/1, print_exports/1]).

-define(ASNI_ITALIC, "\e[35;1m"). % Magenta bright
-define(ASNI_BOLD, "\e[36;1m").   % Cyan bright
-define(ASNI_END, "\e[0m").
-define(DEFAULT_WIDTH, 75).
-define(DEFAULT_INDENT, 4).

-spec print([string()]) -> ok.
print(Lines) ->
    print(Lines, [], [0], false).

-spec print_exports(atom()) -> ok.
print_exports(Module) ->
    Exports = Module:module_info(exports),
    Col = col(Exports, []),
    N = ?DEFAULT_WIDTH div Col,
    print_exports(lists:sort(Exports), Col, N, 1).

print_exports([], _, _, _) ->
    io:format("~n~n");
print_exports([{F, A} |R], Col, NCol, N) ->
    io:format("~*s", [-Col, lists:concat([F, "/", A])]),
    if
        NCol == N ->
            io:format("~n"),
            print_exports(R, Col, NCol, 1);
        true ->
            print_exports(R, Col, NCol, N + 1)
    end.

col([], Acc) ->
    lists:max(Acc) + 5;
col([{F, _}|R], Acc) ->
    col(R, [length(atom_to_list(F)) | Acc]).

print([], _Buffer, _Indent, _Fill) ->
    ok;
print(["" | R], Buffer, Indent, Fill) ->
    flush(Buffer, lists:sum(Indent), Fill),
    print(R, [], Indent, Fill);
print([".br" | R], Buffer, Indent, Fill) ->
    flush(Buffer, lists:sum(Indent), Fill),
    print(R, [], Indent, Fill);
print([".fi" | R], Buffer, Indent, Fill) ->
    flush(Buffer, lists:sum(Indent), Fill),
    print(R, [], Indent, true);
print([".nf" | R], Buffer, Indent, Fill) ->
    flush(Buffer, lists:sum(Indent), Fill),
    print(R, [], Indent, false);
print([".RS"|R], Buffer, Indent, Fill) ->
    flush(Buffer, lists:sum(Indent), Fill),
    print(R, [], [4|Indent], Fill);
print([".RE"|R], Buffer, [], Fill) ->
    flush(Buffer, 0, Fill),
    print(R, [], [], Fill);
print([".RE"|R], Buffer, Indent, Fill) ->
    flush(Buffer, lists:sum(Indent), Fill),
    [_|I] = Indent,
    print(R, [], I, Fill);
print([[$.,$R, $S,$\s|N] | R], Buffer, Indent, Fill) ->
    flush(Buffer, lists:sum(Indent), Fill),
    print(R, [], [list_to_integer(N) | Indent], Fill);
print([".LP" | R], Buffer, Indent, Fill) ->
    flush(Buffer, lists:sum(Indent), Fill),
    io:format("~n"),
    print(R, [], Indent, true);
print([[$.,$T,$P|N] |R], Buffer, Indent, Fill) ->
    flush(Buffer, lists:sum(Indent), Fill),
    R1 = flush_tp(R, N, lists:sum(Indent)),
    print(R1, [], Indent, Fill);
print([[$.|_] | R], Buffer, Indent, Fill) ->
    print(R, Buffer, Indent, Fill);
print([Line | R], Buffer, Indent, true) ->
    Buf = Buffer ++ string:lexemes(Line, " "),
    print(R, Buf, Indent, true);
print([Line | R], Buffer, Indent, false) ->
    print(R, Buffer ++ [Line], Indent, false).

flush([], _Indent, _FillMode) ->
    ok;
flush(Words, Indent, true) ->
    {Line, R1} = fill_line(Words, ?DEFAULT_WIDTH - Indent, []),
    io:format("~.*c~s~n", [Indent, $\s, string:join(Line, " ")]),
    flush(R1, Indent, true);
flush([Line | R], Indent, false) ->
    L1 = string:replace(Line, "\\&", "", all),
    io:format("~.*c~ts~n", [Indent, $\s, list_to_binary(L1)]),
    flush(R, Indent, false).

flush_tp(R, N, Indent) ->
    TWidth = list_to_integer(string:trim(N)),
    {Tag, R1} = get_tag(R),
    {Par, R2} = get_par(R1, []),
    if
        length(Tag) > TWidth ->
            io:format("~.*c~s~n", [Indent, $\s, escape_word(Tag)]),
            flush(Par, TWidth + Indent, true);
        true ->
            Width = ?DEFAULT_WIDTH - Indent - TWidth,
            {Line, P2} = fill_line(Par, Width, []),
            io:format("~.*c~.*s~s~n", [Indent, $\s, TWidth, Tag, string:join(Line, " ")]),
            flush(P2, TWidth + Indent, true)
    end,
    R2.

fill_line([], _Width, Acc) ->
    {lists:reverse(Acc), []};
fill_line([W|R] = R0, Width, Acc) ->
    Len = wordlen(W, 0),
    if
        Len >= Width ->
            {lists:reverse(Acc), R0};
        true ->
            W1 = escape_word(W),
            fill_line(R, Width - Len - 1, [W1 | Acc])
    end.

get_par([[$.|_]|_] = R, Acc) ->
    {Acc, R};
get_par([Line |R], Acc) ->
    get_par(R, Acc ++ string:lexemes(Line, " ")).

get_tag([".B" |R]) -> get_tag(R);
get_tag([".I" |R]) -> get_tag(R);
get_tag([T | R])   -> {T, R}.

wordlen([], Len) ->
    Len;
wordlen([$\\, $f, $I|R], Len) ->
    wordlen(R, Len);
wordlen([$\\, $f, $B|R], Len) ->
    wordlen(R, Len);
wordlen([$\\, $f, $R|R], Len) ->
    wordlen(R, Len);
wordlen([$\\, $&|R], Len) ->
    wordlen(R, Len);
wordlen([_C | R], Len) ->
    wordlen(R, Len + 1).

escape_word([$\\, $f, $I | R]) ->
    cleanup_escape(?ASNI_ITALIC ++ R);
escape_word([$\\, $f, $B | R]) ->
    cleanup_escape(?ASNI_BOLD ++ R);
escape_word(W) ->
    cleanup_escape(W).

cleanup_escape(W0) ->
    W1 = string:replace(W0, "\\fR\\&", ?ASNI_END),
    W2 = string:replace(W1, "\\fI", "", all),
    W3 = string:replace(W2, "\\fB", "", all),
    W4 = string:replace(W3, "\\fR", "", all),
    W5 = string:replace(W4, "\\&", "", all),
    string:trim(W5).
