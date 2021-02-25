## Compiler by bmateusz

![Scala CI](https://github.com/bmateusz/compiler/workflows/Scala%20CI/badge.svg)

Recreational programming in Scala for fun.

### [Try it out in the browser](https://bmateusz.github.io/compiler/)

## Build instructions

* Run tests `sbt test`
* Test coverage: `sbt clean "project compiler" coverage test coverageReport`
* Run CLI REPL: `sbt "compiler/runMain Main"`
* Run Swing UI: `sbt "compiler/runMain Swing"`
* Scala JS: `npm --prefix js install` to fetch monaco editor, `sbt ~compilerJS/fastLinkJS` and open js/index.html
* Scala Native: `sbt ~compilerNative/nativeLink` and run `./native/target/scala-2.13/compiler-out`

## Example code

```

```
