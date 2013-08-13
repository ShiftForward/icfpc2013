```
// ****************************************************************
// *   Software Failure.  Press left mouse button to continue.    *
// *             Guru Meditation #00000000.00000000               *
// ****************************************************************
```

## What is this?

This is Guru Meditation's entry to the
[ICFP 2013 Contest](http://icfpc2013.cloudapp.net/).

## Who we are

* André Silva ([ShiftForward][shiftforward])
* Hugo Sereno Ferreira ([ShiftForward][shiftforward], [FEUP][feup])
* Joao Azevedo ([ShiftForward][shiftforward])
* Rui Gonçalves ([ShiftForward][shiftforward])

## Some statistics

* **629** problems solved;
* **1068** lines of Scala;
* **30** hours spent working on the contest, from 9 to 11 August.
* **1** machine (Macbook Pro) with 16Gb of RAM.

## Our approach

### Friday

We never discussed the language we should write our entry in. Scala was the
obvious choice, so we started by creating the infrastructure to interact with
the API, using [spray][spray.io] to build the client. We, however, didn't
dedicate much time to the contest on Friday. 

 * **Noon.** Around this time we had enough code to create \BV ASTs. Our initial thought for finding the correct programs
was to map the outputs to a linear function known from the used operators. That
strategy was implemented in our `PlotSolver` and was able to successfully solve
all size 3 problems. 

 * **Night.** We created the parser of \BV programs, the
\BV to Scala compiler, and our initial program generator. The parser used
Scala's [parser combinators][par-comb], and ended up very short in size. Our
program generator created Scala streams of valid expressions, given a program
size and a set of operators to use, using [sequence comprehensions][seq-comp].

### Saturday

 * **Morning.** We had the first version of the `BruteForceSolver` that
simply generated all valid programs and tested them against the given
inputs. Our generator didn't incorporte many optimizations by then, and various
syntactically equal programs were being generated. Nevertheless, we were able to
solve all problems of size up to 8 with that preliminary version. Henceforth, we dedicated
the major part of our time optimizing the
generator, by avoiding the creation of syntactically (and semantically) equal
programs. We avoided the generation of repeated expressions in binary operations
(that were all commutative), and dealt with the absorbing elements of the
various operators. 

 * **Night.** Each of our expressions had a
`staticValue` property that would hold, if the expression didn't have variables,
the value that was able to be computed from it. With those optimizations we were 
able to solve all problems of size up to 12. During that day, we also had the idea 
of generating a sort of [rainbow tables][rainbow] that would map the hash of 
outputs of a collection of known inputs to generated programs, so that a matching
program could be found by simply comparing hashes of outputs. We implemented a
preliminary version of them during that night, and left it running while we slept. It
turned out we had *huge* bottlenecks inserting to a SQLite database, so that did 
us no good.

### Sunday

Sunday was all about optimizing the generator and trying to get the
`RainbowTableSolver` usable. We avoided the generation of expressions that
yielded the same static value, which reduced our search space significantly, and
also implemented a cache of smaller expressions (that had a bug with fold
expressions). Those optimizations together allowed us to crank most of the size
13 and 14 problems, as well as problems with `tfold` up to size 16. We also did
a bunch of optimizations in bitwise operations before we figured out it would be
best if we performed a feature freeze and tried to solve the remaining
problems. By then, we had a usable `RainbowTableSolver`, and prepopulated the
table with programs of size up to 7. Our idea was to run the solver for the
remaining problems, adding new generated programs to the table. 

* **10pm UTC.** Around this time, with merely 2 hours left, we had more that 1800 problems
remaining. We planned our final assault poorly, so we decided to try and leave
the solver running with only the programs it had on the table, avoiding the
costly step of generating new ones. That yielded us around 170 extra points, but
we ran out of time with a massive remaining of about 998.

### Optimizations

We attempted to took advantage of some identities to reduce the search space, particularly:

* Commutativity of `and/or/xor/plus`;
* Double negation `(not (not x)) == x`;
* Neutral elements (e.g. `0`, `0xFFFFFFFFFFFFFFFF`) `and/or/xor/plus/shl1/shr1/shr4/shr16`;
* De Morgan Laws;
* Same parameters in binary ops `xor/and/or`;
* IFs: `(if0 a a 0) == 0`, `(if0 a 0 a) == a` and `(if0 a b b) == b`.
 
And a couple more...

### Post-contest

By the end of the contest, we were regretting the fact that we didn't start
running the solver for the remaining problems earlier, even with only a bunch of
smaller programs precalculated. We believe that the `RainbowTableSolver`, as it
was, could have given us easily at least 200 points more. That thought was also
supported later on, when [jrudolph][jrudolph]
[published some stats][jrudolph-pm] that stated that the average simplified
solution size was around 9. We also didn't have a proper strategy to handle the
bonus problems. We were able to figure out their structure, and include it in
our generator, but we didn't come up with the idea of transforming bonus
problems in the problem of generating two different (and smaller) programs that
together, and with the proper `if` condition, would generate the target program.
We also never actively pursued an effort in supporting more than 1 machine to 
cooperatively solve the problems, though it would be relatively easy with the usage
of [Akka][akka] framework.

Overall, We believe we had a lot more fun than initially thought after reading
the problem statement, and the experience of this year's contest was very
positive.

[feup]: http://www.fe.up.pt
[shiftforward]: http://shiftforward.eu
[akka]: http://akka.io/
[spray.io]: http://spray.io/
[seq-comp]: http://docs.scala-lang.org/tutorials/tour/sequence-comprehensions.html
[par-comb]: http://www.scala-lang.org/api/current/index.html#scala.util.parsing.combinator.Parsers
[rainbow]: http://en.wikipedia.org/wiki/Rainbow_table
[jrudolph]: http://github.com/jrudolph
[jrudolph-pm]: http://gist.github.com/jrudolph/83afde5c992bd94666c8
