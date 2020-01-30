### eman

---

A library to read Erlang man page in erl shell.

---

### Usage
Add erlang man directory to environment variable MANPATH, i.e.

```bash
export MANPATH=`man --path`:/usr/local/lib/erlang/man
```

##### eman:h(Module::atom()) -> ok.

```erlang
> eman:h(lists).
  lists <stdlib 3.6>

  Summary
  ...

  Exports
  ...
```

##### eman:h(Module::atom(), Fun::atom()) -> ok.

```erlang
> eman:h(lists, append).
  append(ListOfLists) -> List1
  ...

  append(List1, List2) -> List3
  ...

```

##### eman:h(Module::atom(), Fun::atom(), Arity::integer()) -> ok.

```erlang
> eman:h(lists, append, 1).
  append(ListOfLists) -> List1
  ...

```

### License
[Apache 2.0] (https://github.com/ksyu/eman/blob/master/LICENSE)
