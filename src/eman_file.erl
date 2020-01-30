-module(eman_file).

-export([read/1]).

-spec read(atom() | string()) -> {ok, string()} | {error, not_found}.
read(Module) ->
    try
        {ok, Dir} = erl_man_dir(),
        Path = filename:join([Dir, "man3"]),
        Name = lists:concat([Module, ".*"]),
        {ok, File} = filelib:find_file(Name, Path),
        {ok, Bin} = read_file(File, filename:extension(File)),
        string:split(binary_to_list(Bin), "\n", all)
    of
        Lines -> {ok, Lines}
    catch
        _:Reason -> {error, Reason}
    end.

read_file(File, ".gz") ->
    {ok, Bin} = file:read_file(File),
    {ok, zlib:gunzip(Bin)};
read_file(File, ".3") ->
    file:read_file(File).

manpath() ->
    case os:getenv("MANPATH") of
        false -> os:cmd("man --path");
        Path  -> Path
    end.

find_man_dir() ->
    Path = manpath(),
    Fun = fun(P) ->
                  Dir = filename:join([P, "man3"]),
                  case filelib:find_file("erlang.*", Dir) of
                      {ok, _} -> true;
                      _Error  -> false
                  end
          end,
    case lists:filter(Fun, string:split(Path, ":", all)) of
        [] -> {error, not_found};
        [P|_] -> {ok, P}
    end.

erl_man_dir() ->
    case get(manpath) of
        undefined ->
            {ok, Path} = find_man_dir(),
            put(manpath, Path),
            {ok, Path};
        Path ->
            {ok, Path}
    end.
