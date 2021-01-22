## Compiler by bmateusz

![Scala CI](https://github.com/bmateusz/compiler/workflows/Scala%20CI/badge.svg)

Recreational programming in scala for fun.

## Build instructions

* Run tests `sbt test`
* Test coverage: `sbt clean coverage test coverageReport`
* Run CLI REPL: `sbt "compiler/runMain Main"`
* Run Swing UI: `sbt "compiler/runMain Swing"`
* Scala JS: `sbt ~compilerJS/fastLinkJS` and open index.html
* Scala Native: `sbt ~compilerNative/nativeLink` and run `./native/target/scala-2.13/compiler-out`

## Example code

```

```
