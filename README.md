# cljcc

A toy C Compiler implementation in Clojure. 

Browser version: [cljcc](https://cljcc.shagunagrawal.me)

Follows the book [Writing a C Compiler by Nora Sandler](https://nostarch.com/writing-c-compiler).

Post about my experience implementing the book [writing-a-c-compiler-in-clojure](https://shagunagrawal.me/posts/writing-a-c-compiler-in-clojure/).

Post on how to compile to WASM [compiling-clojure-to-webassembly](https://shagunagrawal.me/posts/compiling-clojure-to-wasm-image/).

## Prerequisites

* [Clojure](https://clojure.org)
* [GraalVM](https://www.graalvm.org) for building native binary
* [babashka](https://github.com/babashka/babashka#installation) 
    
Only Linux and Mac OS is supported. For Windows, run through WSL. 
    
## Tasks

To see all available tasks in the project, run `bb tasks`:

``` sh
bb tasks
The following tasks are available:

clean            Removes target folder.
nrepl            Starts a nrepl session.
storm            Starts a nrepl session with storm debugger.
lib:run:main     Run's main only for cljcc library.
lib:build:jar    Builds uberjar only for cljcc lib.
lib:build:wasm   Builds native WASM image.
cli:run:main     Run's main CLI function.
cli:build:jar    Builds uberjar for CLI.
cli:run:jar      Runs CLI jar.
cli:build:native Builds native image for CLI.

```

## Build

To build native image, run:

( This requires having `native-image` tool installed locally. Use the appropriate JDK distribution. )

``` sh
bb cli:build:native
```

This produces a binary `cljcc-cli` at `/target/cli`. Pass the path to the C file.

``` sh
./target/cli/cljcc-cli "path/to/file.c"
```

## Run Jar

``` sh
bb cli:run:jar
```

## References

Some talks / projects which helped in implementation.

* [What's So Hard About Writing A Compiler, Anyway? Oh - Ramsey Nasser](https://www.youtube.com/watch?v=_7sncBhluXI)
* [Clojure Lexer implementation by Vikasg7](https://github.com/ThePrimeagen/ts-rust-zig-deez/tree/master/clj)


