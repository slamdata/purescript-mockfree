# purescript-mockfree

`mockfree` is a purely-functional, strongly-typed mocking library for all PureScript programs that are defined by [Free](https://github.com/purescript/purescript-free) algebras.

While a proof-of-concept for a [LambdaConf 2016 talk](http://github.com/lambdaconf/lambdaconf-2016-usa), the library is nonetheless completely usable, and demonstrates the power of modeling effectful computation through descriptive data structures.

- [Module Documentation](docs/Test/Mock/Mockfree.md)
- [Example](test/Main.purs)

## Building

    bower install purescript-mockfree
    pulp build
    pulp test

## Introduction

Integration and system testing often require writing tests that perform *effects*, such as writing to a file system or connecting to a remote API.

The problem with such tests is that they verify far more than the logic of your program: they test the reliability of third-party software and systems.

As a result, integration and system tests are often indeterministic and error-prone. In addition, because these tests often run slowly and have complex dependencies, they are not as useful to developers as unit and property tests, which run very fast and have no dependencies.

The industry has invented *mocking* as a solution to these problems. However, mocking usually sacrifices type safety, and requires a powerful dynamic programming language, or lots of hacks or code rewriting.

With a purely-functional programming language such as PureScript, we have an alternative: `Free` programs.

`Free` programs allow us to describe the effects of our program using data structures. These data structures can later be interpreted into effectful operations. However, they can also be interpreted into *non-effectful* operations that do *not* interact with external systems.

If you program in this style, then `mockfree` will let you trivially test the logic of your programs with purely-functional, strongly-typed mocks, which can be parallelized, which run in-memory, and which are completely deterministic.

Combined with a way of testing your program's final interpreters, functional mocking can totally transform the way you test functional code that has complex interactions with external systems.

With `Free`, you can test final interpreters separately, one time, in their own library; and you can test your program logic exclusively through functional mocking. All tests become fast, deterministic, and completely self-contained!

## Tutorial

Let's define a console program with the following operations:

```purescript
data ConsoleF a
  = WriteLine (Op String Unit   a)
  | ReadLine  (Op Unit   String a)
```

Each operation `Op a b c` requires that we supply an `a` in order to get access to a `b`.

For example, a "write line" operation requires that we supply a `String`, and gives us access to a `Unit` value (i.e. no information), while a "read line" operation requires that we supply a `Unit` (i.e. no information) to get access to a `String`.

The `mockfree` library requires that we define polymorphic [prisms](http://github.com/purescript-contrib/purescript-profunctor-lenses) for each term in our operational algebra:

```purescript
_WriteLine :: OpPrism ConsoleF String Unit
_ReadLine :: OpPrism ConsoleF Unit String
```

While these prisms are required for `mockfree`, they are also useful for working with our algebra, and they can be shared with production code!

Once the prisms have been defined, we can create a mock spec using some combination of `expect` (for read-write operations), `expectRead` (for read-only operations), and `expectWrite` (for write-only operations):

```purescript
mockSpec :: MockSpec ConsoleF
mockSpec = do
  expectWrite _WriteLine (assertEquals "What is your name?")
  expectRead  _ReadLine  "World"
  expectWrite _WriteLine (assertEquals "Hello, World!")
```

These specs can be run against a program using `runMock`:

```purescript
runMock mockSpec program :: Either String Unit
```

where `program` is a program defined by sequential execution of the individual operations:

```purescript
program :: Free ConsoleF Unit
program = do
  writeOp _WriteLine "What is your name?"
  name <- readOp _ReadLine
  writeOp _WriteLine ("Hello, " ++ name ++ "!")
```

## Future Work

There are several ways this library could be improved:

 1. **Add support for branching programs.** Currently, the mock spec is a linear sequence of instructions. Ideally, it would be a tree that forks based on runtime values and allows alternatives.
 2. **Add support for infinite mock specs.** Currently, the mock spec can only model finite, bounded programs.
 3. **Factor out Assertion into a library.** Currently, there is no PureScript library for non-effectful assertions that generate nice, composable errors.
 4. **Factor out the Op type & helpers into a library.** These could be useful in building `Free` programs, not just in testing them.
