-module(eman).

-export([e/1, h/1, h/2, h/3]).

-spec e(atom()) -> ok.
e(Module) ->
    eman_io:print_exports(Module).

-spec h(atom()) -> ok.
h(Module) ->
    {ok, Lines} = eman_file:read(Module),
    #{application := App, summary := Text} = eman_parse:parse(Lines),
    io:format("~s <~s>~n", [Module, App]),
    eman_io:print(Text),
    io:format("Exports:~n~n"),
    eman_io:print_exports(Module).

-spec h(atom(), atom()) -> ok.
h(M, F) ->
    {ok, Lines} = eman_file:read(M),
    Man = eman_parse:parse(Lines),
    Doc = find(Man, atom_to_list(F)),
    print_doc(Doc, lists:concat([M, ":", F])).

-spec h(atom(), atom(), pos_integer()) -> ok.
h(M, F, A) ->
    {ok, Lines} = eman_file:read(M),
    Man = eman_parse:parse(Lines),
    Doc = find(Man, F, A),
    print_doc(Doc, lists:concat([M,":", F, "/", A])).

print_doc([], _MFA) ->
    ok;
print_doc(false, MFA) ->
    io:format("No documentation for ~s was found~n", [MFA]);
print_doc([D|R], MFA) ->
    eman_io:print(D),
    print_doc(R, MFA).

-spec find(map(), atom(), integer()) -> list() | false.
find(#{exports := Exports, index := Index}, Fun, Arity) ->
    case lists:keyfind({Fun, Arity}, 1, Index) of
        {{Fun, Arity}, N} -> [lists:nth(N, Exports)];
        false -> false
    end.

-spec find(map(), string()) -> list().
find(#{exports := Exports}, Name) ->
    Fun = fun(F, Acc) ->
                  [N |_] = string:split(hd(F), "("),
                  if
                      Name == N -> Acc ++ [F];
                      true -> Acc
                  end
          end,
    case lists:foldl(Fun, [], Exports) of
        []    -> false;
        Found -> Found
    end.
