# lys_language
Part II Project 2022/2023: Type-safe multi-stage programming with Lys

## Setup & Run the Lys interpreter

First, install the dependencies:
```sh
opam install
```

Then, to execute a lys program:
```sh
dune exec lys -- FILENAME -interpreter [m|mt|s|ss|ssv]
```

and to open the REPL:
```sh
dune exec lys_repl -- -interpreter [m|mt|s|ss|ssv] -withfile FILENAME
```

Available interpreters:
- `m`: multi-step interpreter
- `mt`: multi-step interpreter instrumented with execution time information
- `s`: single-step interpreter
- `ss`: single-step interpreter with reduction step count
- `ssv`: single-step interpreter with reduction step count and in verbose mode (so with a dump of the reduction steps)

## License
This project is dual-licensed under both the MIT License and the Apache License (Version 2.0). See [LICENSE-APACHE](./LICENSE-APACHE) and [LICENSE-MIT](./LICENSE-MIT) for details.
