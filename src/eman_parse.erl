-module(eman_parse).

-export([parse/1]).

-spec parse([string()]) -> map().
parse(Content) ->
    {Mod, App} = title(hd(Content)),
    Sections = split_section(tl(Content), []),
    Exports = get_exports(Sections),
    Index = index_exports(Exports, 1, []),
    Desc = get_val("description", Sections),
    #{application => App,
      module => Mod,
      summary => Desc,
      exports => Exports,
      index => Index
     }.

% {{Fun::atom(), Arity::integer()} | FunName::string(), Nth::integer()}.
index_exports([], _Nth, Acc) ->
    Acc;
index_exports([F|R], Nth, Acc) ->
    index_exports(R, Nth + 1, Acc ++ index(F, Nth, [])).

index([], _Nth, Acc) ->
    Acc;
index(["" |R], Nth, Acc) ->
    index(R, Nth, Acc);
index([[$.|_] |R], Nth, Acc) ->
    index(R, Nth, Acc);
index([Line | R], Nth, Acc) ->
    case func_arity(Line) of
        {arity, {erlang, F, A}} ->
            index(R, Nth, [{{F, A}, Nth} | Acc]);
        {arity, Arity} ->
            index(R, Nth, [{Arity, Nth} | Acc]);
        {func, Fun} ->
            index(R, Nth, [{Fun, Nth} | Acc]);
        {not_func, _String} ->
            Acc
    end.

title([$.,$T,$H,$\s|R0]) ->
    [M, R1] = string:split(R0, " "),
    [_, R2] = string:split(R1, " "),
    {A, _R} = scan_quote(tl(R2), $", []),
    {M, A}.

split_section([], Acc) ->
    lists:reverse(Acc);
split_section(Lines, Acc) ->
    {S, R} = section(Lines, {}),
    split_section(R, [S | Acc]).

section([], {Title, Text}) ->
    {{Title, lists:reverse(Text)}, []};
section([[$.,$S,$H|T]|R], {}) ->
    section(R, {section_title(T), []});
section([[$.,$S,$H|_]|_] = R, {Title, Text}) ->
    {{Title, lists:reverse(Text)}, R};
section([Line|R], {T, Text}) ->
    section(R, {T, [Line | Text]}).

section_title(Words) ->
    Ws = string:lowercase(string:trim(Words)),
    case hd(Ws) of
        $" -> lists:droplast(tl(Ws));
        _  -> Ws
    end.

get_exports(Sections) ->
    Exports = proplists:get_all_values("exports", Sections),
    get_exports(Exports, []).

get_exports([], Acc) ->
    Acc;
get_exports([Text | R], Acc) ->
    Fs = split_func(Text),
    get_exports(R, Acc ++ Fs).

%% erlang functions with return type: func(args) -> return
%% erlang functions missing return type, i.e. debugger.3: func(args)
%% c functions
is_func(Line) ->
    L = string:replace(Line, "\n", " ", all),
    {ok, P1} = re:compile("^[a-zA-Z0-9_:]+\\(.*\\)\s*->"),
    {ok, P2} = re:compile("^[a-zA-Z0-9_]+\\([^(]*\\)"),
    is_func(L, P1) orelse is_func(L, P2) orelse is_c_func(L).

is_c_func(Line) ->
    {ok, P} = re:compile("^[a-zA-Z0-9_]+\s+[a-zA-Z0-9_]+\\(.*\\)"),
    is_func(Line, P).

is_func(Line, Pattern) ->
    case re:run(Line, Pattern) of
        nomatch    -> false;
        {match, _} -> true
    end.

patch(F0) ->
    F1 = string:replace(F0, "\n", " ", all),
    F2 = string:replace(F1, "\\fI", "", all),
    F3 = string:replace(F2, "\\fB", "", all),
    F4 = string:replace(F3, "\\fR", "", all),
    F5 = string:replace(F4, "\\&", "", all),
    F6 = string:replace(F5, ">= 0", "", all),
    F7 = string:replace(F6, ">= 1", "", all),
    F8 = patch(F7, "^Mod:|^Module:|^FModule:|^HModule:", ""),
    F9 = patch(F8, "\\,\s+\\[\\,\s*[^]]+\\]", ""),
    F10 = patch(F9, "\\[\\,\s*[^]]+\\]", ""),
    string:trim(F10).

patch(Fun, Pattern, Replace) ->
    {ok, P} = re:compile(Pattern),
    case re:run(Fun, P) of
        {match, _} -> re:replace(Fun, P, Replace, [{return, list}]);
        nomatch -> Fun
    end.

% func_arity(Fun) -> {func, string()}     |
%                    {not_func, string()} |
%                    {arity, {atom(), integer()}}
func_arity(Fun) ->
    F = patch(Fun),
    case is_func(F) of
        true ->
            case is_c_func(F) of
                true -> {func, F};
                false -> parse_arity(F)
            end;
        false ->
            {not_func, F}
    end.

parse_arity(Fun) ->
    try
        [Clause | _] = string:split(Fun, "->"),
        Spec = "-spec " ++ Clause ++ " -> ok.",
        {ok, Tokens, _} = erl_scan:string(Spec),
        {ok, Form} =  erl_parse:parse_form(Tokens),
        erl_syntax_lib:analyze_wild_attribute(Form)
    of
        {spec, {S, _}} -> {arity, S}
    catch
        _:_ -> {func, Fun}
    end.

has_content([]) ->
    false;
has_content([[$.|_]|R]) ->
    has_content(R);
has_content(["" |R]) ->
    has_content(R);
has_content([L | R]) ->
    case is_func(L) of
        true  -> has_content(R);
        false -> true
    end.

split_func(Exports) ->
    Lines = lists:dropwhile(fun(L) -> L /= ".B" end, Exports),
    split_func(Lines, []).

split_func([], Acc) ->
    lists:reverse(Acc);
split_func(Lines, Acc) ->
    {Func, R1} = func(Lines, []),
    split_func(R1, [Func | Acc]).

%% scan one function reference
func([], Acc) ->
    {lists:reverse(Acc), []};
func([".B" | R], []) ->
    {Clause, R1} = scan_func_clause(R, []),
    case is_func(Clause) of
        true  -> func(R1, [Clause]);
        false -> func(R, [])
    end;
func([".B" | R] = R0, Acc) ->
    {Clause, R1} = scan_func_clause(R, []),
    case is_func(Clause) of
        true ->
            case has_content(Acc) of
                true  -> {lists:reverse(Acc), R0};
                false -> func(R1, [Clause | Acc])
            end;
        false ->
            func(R, Acc)
    end;
func([Line|R], Acc) ->
    func(R, [Line | Acc]).

scan_func_clause([".B" | R], Acc) ->
    scan_func_clause(R, Acc);
scan_func_clause([[$.|_]|_] = R, Acc) ->
    Name = string:join(lists:reverse(Acc), "\n"),
    {Name, R};
scan_func_clause([L | R], Acc) ->
    scan_func_clause(R, [L | Acc]).

get_val(Key, Sections) ->
    case lists:keyfind(Key, 1, Sections) of
        false -> [];
        {Key, Val} -> Val
    end.

scan_quote([], _Q, Acc) ->   {lists:reverse(Acc), []};
scan_quote([Q|R], Q, Acc) -> {lists:reverse(Acc), R};
scan_quote([C|R], Q, Acc) -> scan_quote(R, Q, [C | Acc]).
