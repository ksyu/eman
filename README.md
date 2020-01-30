### erlman

---

A library to read Erlang man page in erl shell.

---

### Usage
Add erlang man directory to environment variable MANPATH, i.e.

```bash
export MANPATH=`man --path`:/usr/local/lib/erlang/man
```

##### erlman:h(Module::atom()) -> ok.

```erlang
> erlman:h(lists).
  lists <stdlib 3.6>

  Summary
  ...

  Exports
  ...
```

##### erlman:h(Module::atom(), Fun::atom()) -> ok.

```erlang
> erlman:h(lists, append).
  append(ListOfLists) -> List1
  ...

  append(List1, List2) -> List3
  ...

```

##### erlman:h(Module::atom(), Fun::atom(), Arity::integer()) -> ok.

```erlang
> erlman:h(lists, append, 1).
  append(ListOfLists) -> List1
  ...

```

### License
[Apache 2.0] (https://github.com/ksyu/erlman/blob/master/LICENSE)
