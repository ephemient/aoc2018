# [Advent of Code 2018](https://adventofcode.com/2018)
### my answers in [Kotlin](https://www.kotlinlang.org/) (see also [Haskell branch](https://github.com/ephemient/aoc2018/tree/master))

[![Build Status](https://travis-ci.org/ephemient/aoc2018.svg?branch=kotlin)](https://travis-ci.org/ephemient/aoc2018)

This project builds with [Gradle](https://gradle.org/).

Run the [JUnit 5](https://junit.org/junit5/) test suite:

```sh
./gradlew test
```

Run [JMH](https://openjdk.java.net/projects/code-tools/jmh/) benchmarks:

```sh
./gradlew jmh
```

Print solutions for the inputs provided in local data files:

```sh
./gradlew run
```

Generate [Dokka](https://github.com/Kotlin/dokka) API documentation
(rendered by [JitPack](https://jitpack.io/com/github/ephemient/aoc2018/kotlin-SNAPSHOT/javadoc/aoc2018/index.html)):

```sh
./gradlew dokka
```

Run [ktlint](https://ktlint.github.io/) Kotlin linter:

```sh
./gradlew lintKotlin
```
