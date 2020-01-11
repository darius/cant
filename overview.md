## Design goals

As the README says, I want a self-explaining computing system. Such a
system needs a high-level programming language. Cant is meant to be
both the main language you use in this fantasized environment and the
language it's written in (except at its lowest level). For the whole
system to be simple enough to learn completely, Cant must be, too --
much simpler than, say, Python, which is easy to start learning but
rather complex in sum. Most Python programmers don't know all of
Python.

Since this FantasyOS ought to support capability security, so should
this language (though it doesn't quite yet). My first try at a
capability-secure Scheme dialect,
[consp](https://github.com/darius/consp), persuaded me it's important
that "all you can do is send a message" (or almost all). That is,
Scheme has plenty of data types, plus different operations on each
type, and the operations insist you pass them an object of just the
type they want, never some wrapper you just made up. In Cant, most
operations are like function calls instead: you send an object a
message, the receiver decides what to do with it, and you're mostly
free to substitute your own objects that interpret the message their
own way.

This environment should make sense in its own terms: the
implementation's internal state should be viewable and to some extent
manipulable from Cant itself, as Cant objects.

I have prejudices about what makes code readable, and I've indulged
them. This indulgence goes to a frankly silly degree of abandoning
familiar Scheme names and syntax which were not really a problem, such
as `lambda`, `car`, `cdr`, and way beyond that. I felt a need to
bikeshed long-settled conventions just to open up a space where
more-significant new ideas might come in -- which I guess mostly
haven't, though maybe they will to you.


## What else changed from Scheme?

Collections follow a common protocol. Every collection is a kind of
map. (Lists are sequences, which are maps from a natural-number index
to a value (plus some extra methods).)

But Cant is not class-based or prototype-based; the expression to
make an object is more like a Scheme lambda-expression. It's sort of
like the message-passing objects in SICP chapter 3, but streamlined,
with pattern-matching. (See the E programming language for the actual
inspiration.) Instead of inheritance there are 'traits'.

Patterns are supported everywhere you can bind a variable. Definitions
work the same at top level as in nested scopes. The scope of a binding
is the whole sub-block it's in -- recursion and forward references
aren't special. (A use-before-define error can happen at runtime.)
Variables are immutable. (There's a 'box' type for when you need a
mutable variable; also, for development/debugging, you can update
definitions in an 'interactive setting' such as the usual listener.
XXX redefinitions are broken again)

Lists and strings are immutable too. Currently the built-in hashmaps,
arrays, etc. are mutable, though I think now they shouldn't be. (As a
placeholder towards that future, there's an immutable map type, though
it's an association list under the hood.)

Scheme has lots of undefined behavior; in Cant the ideal is for any
nondeterminism to be explicit. Since there's no spec, this is
currently just a matter of what style of language we're aiming for
someday.

I said that almost all you can do is send a message. A big exception
is testing for equality with `=`. For objects you create with `make`,
equality is identity, as with Scheme's `eq?`. For built-in pure data,
and compound data called 'terms', equality is structural. I want to
support creating new kinds of pure-data objects, but haven't yet got
to it or even fully designed it. (Probably that'll follow E's example
again.)

A procedure call is a special case of sending a message: `(foo bar
baz)`, when `foo` is not defined as syntax (a special form or macro),
is sugar for `(call foo {~ bar baz})`. In turn, `(call receiver
message)` is a special form meaning to evaluate `receiver` and
`message` and send `message` to `receiver`. The form `{~ bar baz}`
evaluates `bar` and `baz` and creates a 2-tuple of the values of `bar`
and `baz`: a kind of 'term' as mentioned above. Tuples and other terms
are distinct from lists.

Other terms are distinguished from tuples by a tag different from `~`.
For example,
[examples/intset2.cant](https://github.com/darius/cant/blob/master/examples/intset2.cant)
has terms like `{empty}` and `{extension n s}`. This example shows
pattern-matching on terms, in a style like ML or Haskell code.

The variation `(foo .bar baz)`, with a `.` starting the first
argument, means `(call foo {.bar baz})`: that is, with `.bar` as the
*term tag* instead of as an argument to a tuple. Think of `{.bar baz}`
as a message to an object, in OO style. Compare
[examples/intset1.cant](https://github.com/darius/cant/blob/master/examples/intset1.cant),
an OO verson of `intset2`.

One more bit of sugar for calls: `foo.bar` is shorthand for `(foo
.bar)` (meaning, again, `(call foo {.bar})`).


## The implementation

It's sort of Smalltalky, where for now the interpreter internals
always appear as meta-objects with no optimization, and even primitive
objects execute via high-level scripts (in `abcs/00-primordia/`). This
is motivated by the goal of a self-sustaining whole system eventually
supporting powerful debugging. To not be incredibly slow, the design
will need to get fancier.

The interpreter and core library, at this writing, amount to ~4000
lines of Scheme and Cant. Back in the 1990s I used a bytecode Scheme
of my own at ~5000 lines of C and Scheme, and that size would not be
too crazy a goal for this... if we stopped counting blank lines and
comments, sigh. Over 10k lines would be disappointing.

It seems hard to find systems anywhere near as small as the
educational toys but with realistic support for real use requiring
debugging, etc. I want to make that system or learn why I can't. (But
'real' here does not include interoperating with other,
already-complex systems.)

There's plenty of unfinished crap in this repo, not clearly marked.


## Quick start for Schemers

Be in this directory (the one holding the file `incant`) and then:

```
$ ./incant
-> (to (I-am name) (out .say "Hi, ~w!\n" name))
#<I-am>
-> (I-am 'Alice)
Hi, Alice!
-> 
```

The less-happy path:

```
-> (I-am 'but 'human)
Error! Traceback:
  (call '#<print-result:repl> ^)
  (^)
Match failure: (#<I-am> (but human))
Enter (debug) for more.
```

That is, `I-am` was called with the message `(but human)` but it
expected a length-1 message like `(Alice)`. (The traceback doesn't
show actual source code, it's barely-readable reconstituted AST data
structures.)

This debugger is almost useless, but it's there as a last resort:

```
-> (debug)
Enter ? for help.
debug-> ?
? help      - this message
q quit      - quit the debugger
r resume    - continue from here, with the value of an expression
u up        - up to caller
d down      - down to callee
e env       - enumerate the variables in the current environment
v value     - evaluate an expression in the current environment
b backtrace - show all of the stack up from here
debug-> q
ok
```

## What to read once you get bored with the following wall of text

There are example programs in
[examples/](https://github.com/darius/cant/tree/master/eg) and
[library/](https://github.com/darius/cant/tree/master/library). To run an
example ([FizzBuzz](https://github.com/darius/cant/blob/master/examples/fizzbuzz.cant)
here):

```
$ ./incant examples/fizzbuzz.cant
```

Or load it in the listener:

```
$ ./incant
-> (load "examples/fizzbuzz.cant")
1
2
Fizz
...

```

The global environment at startup is populated from
[abcs/](https://github.com/darius/cant/tree/master/abcs) (plus a
handful of modules from
[library/](https://github.com/darius/cant/tree/master/library)). Not
that these make great example code necessarily, but they're the first
places to look to clear up questions you may have.

The underlying interpreter in Scheme lives in
[player/](https://github.com/darius/cant/tree/master/player).


## The bikeshedding

Let's start with the equivalents of familiar Scheme syntax:

| Scheme                        | Cant        | Note          |
| ----------------------------- | ------------- | ------------- |
| `(define x 42)`                 | `(let x 42)`       |  Returns 42 as the value. Definitions are expressions.  |
| `(define (f x) e)`              | `(to (f x) e)`     |   |
| `(define (f x) (lambda (y) x))` | `(to ((f x) y) x)` |   |
| `(lambda (x y) e)`              | `(on (x y) e)`     |   |
| `(begin a b c)`                 | `(do a b c)`       |   |
| `(let () a b c)`                | `(hide a b c)`     |   |
| `(if t p q)`                    | `(if t p q)`       | `p` and `q` are in nested scopes, as if in `hide` blocks. |
| `(and x y)`                     | `(and x y)`        | Similarly, `y` is in a nested scope. |
| `(or x y)`                      | `(or x y)`         | Ditto. |
| `(if p (begin a b))`            | `(when p a b)`     | `a` and `b` are in their own common nested scope.  |
| `(if (not p) (begin a b))`      | `(unless p a b)`   |   |
| `(cond (p a) (else b))`         | `(hm (when p a) (else b))`  |  Explained below. |
| `#t`                            | `#yes`  |   |
| `#f`                            | `#no`  |   |
| `(let looping ((v init)) body)` | `(begin looping ((v init)) body)` |  The 'ing' is a convention. |
| `(lambda () e)`                 | `(: e)`     |   |
| `(lambda (it) (turn it 90))`    | `(-> (turn it 90))`     |   |

The `hm` form, like Scheme's `cond`, is complex enough to need
explanation. It typically goes like (from
[examples/games/2048.cant](https://github.com/darius/cant/blob/master/examples/games/2048.cant))
```
(let score (hm (if (lost? board) "You lose!")
               (if forfeit?      "You forfeit.")
               (if (won? board)  "You win!")
               (else             "")))
```
the equivalent to Scheme's
```
(define score (cond ((lost? board) "You lose!")
                    (forfeit?      "You forfeit.")
                    ((won? board)  "You win!")
                    (else          "")))
```

I dropped `cond` for clashing with Cant's style in two ways: the name
is an abbrev instead of an English word, and it lacks syntactic
guideposts to its subparts, making code harder to read when there are
many clauses. (Yes, few clauses are nicer than many clauses, but
sometimes a chain of if-then-elses is just the most direct expression
of the logic.)

You could replace any `cond` with a chain of `if` expressions, but
those would nest and indent progressively to the right. With `hm` you
write the parts linearly instead of nested: `(hm (if a b) c ...)` is
sugar for `(if a b (hm c ...))`. Other kinds of clauses are supported
besides `if`: (from
[abcs/00-primordia/types/string.cant](https://github.com/darius/cant/blob/master/abcs/00-primordia/types/string.cant))

```
(to ~.trim-right
  (begin scanning ((i me.count))
    (hm (when (= i 0)
          "")
        (do (let c (me i.-)))
        (unless c.whitespace?
          (me .slice 0 i))
        (else (scanning i.-)))))
```
corresponding to Scheme
```
(define (trim-right string)
  (let scanning ((i (string-length string)))
    (if (= i 0)
        ""
        (let ((c (string-ref string (- i 1))))
          (if (not (char-whitespace? c))
              (substring string 0 i)
              (scanning (- i 1)))))))
```

I considered making `hm` a naively structural macro which would unnest
any kind of form of its arguments. But that would be more
error-prone. Instead it complains if any of the subforms is not syntax
it knows about: `if`, `when`, `unless`, `do`, `and`, `or`, or `else`. If
there's no `else` clause, then running off the end will cause a
runtime error: so you'll occasionally see code like (from
[examples/circuitoptimizer.cant](https://github.com/darius/cant/blob/master/examples/circuitoptimizer.cant))
```
(hm (when (< gate.+ n-gates)
      (sweeping gate.+))
    (when (= wanted (mask .and value))
      (found? .^= #yes)
      (print-formula L-input R-input))
    (else)))))
```

OK, moving on. Scheme functions on lists:

| Scheme                        | Cant        | Note          |
| ----------------------------- | ------------- | ------------- |
| `(cons x xs)`                 | `(link x xs)`       |  |
| `(append xs ys)`              | `(chain xs ys)`     |  |
| `(list x y z)`                | `(list<- x y z)`    | Though it's more common to use quasiquoting. |
| `(null? xs)`                  | `(null? xs)`, `xs.none?`, or `(xs .none?)`     | `xs.none?` is reader sugar for the last expression. All collections answer this message, though not all objects. `null?` would be useful when you don't know if the argument is a collection. |
| `(pair? x)`                   | `(link? x)`      |  |
| `(list? x)`                   | `(list? x)`     |  Cant doesn't plan to support improper lists, though I haven't got around to making them an error. |
| `(car xs)`                    | `xs.first` or etc.     |  |
| `(cdr xs)`                    | `xs.rest`     |  |
| `(length xs)`                 | `xs.count`     |  |
| `(list-ref xs n)`             | `(xs n)`     |  |
| `(list-tail xs n)`            | `(xs .slice n)`     |  There's also `(xs .slice start-index after-index)` |
| `(member x xs)`               | `(xs .slice (xs .find x))`     |  (Provided `x` is in `xs`. More on `.find` below.) |
| `(cadr (assoc 'x '((a b) (x y))))`   | `((map<-lists '((a b) (x y))) 'x)`     |  More on `map<-` below. |

The accessors on lists above are all generic. They apply to strings too, for a start:

| Scheme                        | Cant        | Note          |
| ----------------------------- | ------------- | ------------- |
| `(string? x)`                 | `(string? x)`       |  |
| `(string a b c)`              | `(string<- a b c)`       |  |
| `(list->string chars)`        | `(string<-list chars)`       |  `chars` may be any sequence. I guess the function's misnamed. Or, really, we should call the concrete type 'link-list' and the abstract one 'list' instead of 'sequence'. TODO? |
| `(string->list s)`            | `s.values`       | `.values` in general returns a sequence which needn't be a link-list: but it should be efficient to walk through with `.first`/`.rest`. |
| `(string-length s)`           | `s.count`     | Just like `.count` on lists. |
| `(string-ref s n)`            | `(s n)`       | Likewise. |
| `(string-append s1 s2)`       | `(chain s1 s2)`       |  |
| `(substring s i1 i2)`         | `(s .slice i1 i2)`     |  |

Same drill with vectors:

| Scheme                        | Cant        | Note          |
| ----------------------------- | ------------- | ------------- |
| `(vector? x)`                 | `(array? x)`       |  |
| `(vector a b c)`              | `(array<- a b c)`       |  |
| `(list->vector xs)`           | `(array<-list xs)`       |  |
| `(make-vector n init)`        | `(array<-count n init)`     |  |
| `(vector->list v)`            | `v.values`       |  |
| `(vector-length v)`           | `v.count`     |  |
| `(vector-ref v n)`            | `(v n)`     |  |
| `(vector-set! v n x)`         | `(v .set! n x)`     | There's nothing like Common Lisp setf, so far. |

So you access a collection by sending a message. What kinds of
collections does the library offer, and what's their protocol?

Collections fit in this hierarchy:
```
  map
    bag
      set
    sequence
      list    -- special in being a 'value' type already. N.B. immutable.
      string  -- ditto
      array, flexarray
      interval, lazy list, enumeration
    grid-2d   -- Just because this came up a lot in Advent of Code;
              -- we don't have general multidimensional arrays as yet.
```

The most general kind of collection, a map `m` from keys to values,
can take the following messages. More-specialized collection types
like bags/sets/sequences also understand the same messages.

| Expression                    | Result          |
| ----------------------------- | ------------- |
| `(m key)`                     | The value for this particular key, or raise an error if it's absent. The primitive `=` function defines sameness between the map's key and the value `key` provided in the message. |
| `(m .get key)`                | Ditto, except the result is `#no` if the key is absent. |
| `(m .get key default)`        | Ditto, except `default` instead of `#no`. |
| `m.none?`                    | Does `m` have any keys? `#yes` or `#no`. |
| `m.keys`                      | A sequence of the keys, in some defined order. If `m` is mutable, the result should present a snapshot of the state as of this call. (For a hashmap, currently, the order is arbitrary, but I intend to make it insertion order when I get around to it.) |
| `m.values`                    | A sequence of the values corresponding to the keys, in the same order. |
| `m.items`                     | A sequence of key-value tuples. (The name `items` is from Python; any ideas for a better name? `mappings`?) |
| `(m .maps? key)`              | Is `key` one of `m`'s keys? |
| `(m .find? value)`            | Is `value` one of `m`'s values? |
| `(m .find value)`             | A key corresponding to `value`, if any; else an error. If `m` is a sequence, then the result should be the *first* corresponding key in `m.items`. I haven't thought about whether that should be expected in general or if it may be any corresponding key at m's discretion. |
| `(m .find value default)`     | Ditto, except if `value` is missing then answer `default`. (Hm, it's a bit of a wart that `.find` doesn't work exactly like `.get` in reverse: i.e. `.get` is not an error on missing.) |
| `m.domain`                    | `m`'s keys as a set. (Maybe not the best name, since 'domain' could be taken to be the type of values the keys must have. Would `key-set` or something be better?) |
| `m.range`                     | `m`'s values as a set. |
| `m.inverse`                   | A map from each of `m`'s values to a corresponding key. If this is ambiguous, then error. |
| `(m .intersects? m2)`         | Do `m` and `m2` have any key in common? |
| `(m .disjoint? m2)`           | Do `m` and `m2` have no key in common? |

In mutable maps:

| Expression                    | Result          |
| ----------------------------- | ------------- |
| `(m .set! key value)`         | Update so `key` maps to `value`. |
| `(m .delete! key)`            | Remove `key` and its value. No effect if `key` already absent. (Should that be an error?) |
| `(m .get-set! key make-value)` | Like `(m key)` except inserting `(make-value)` first if `key` is absent. |
| `m.clear!`                    | Reset the whole map to empty. |

Equality of mutable maps, as for any object that's not pure data, is
by identity.

You can create a mutable hash-map with `(!map<-)` (initially empty),

XXX `(map<-
association-list)`, or `(export name1 name2 ...)` which is like
```(map<- `((name1 ,name1) (name2 ,name2) ...))```.

(Warning: the current implementation in Chez Scheme can't hash
consistently with the equality test, in general. You're safe using
keys that are purely data (such as a link-list all of whose elements
are pure data as well, and so on recursively). A mutable object or
e.g. a list of mutable objects is not expected in this interim
implementation to work, unless you happen not to mutate it. This is
because Chez Scheme doesn't offer a way to make a hashtable keying on
a mix of identity (`eq?`) hashing and a user-defined equality
predicate; it has to be one or the other, and then what do you do with
a key that's a combination? We could've still dealt with this by *not*
representing primitive Cant objects by the same primitive Scheme type,
but that would've created other hassle and expense, which I considered
not worth the cost. The goal of this Cant implementation is to work
out the design, not to be useful. A mythical future real Cant system
needs to define its hashmaps primitively.)

A bag is a kind of mutable map whose values are all counts. (Maybe we
should support negative values too, like Python's `Counter`?) For a
key that's absent from the bag's explicit set of keys, `(bag key)` is
0 rather than an error. `(bag .add! key)` increments the count for
`key`. There are some more convenience methods I'll document later.

(I'll need to revisit all this when we make collections immutable.)

A set is a kind of bag whose nonzero counts are 1. This is another
case of this design prioritizing substitutability over specializing
every subtype's interface.

A list or sequence is a map whose keys are natural numbers. (Cant
calls a natural number a 'count', in its ridiculous stamp-out-jargon
reform campaign.) There are a few extra operations like chaining two
sequences.

You can make lazy sequences with `link/lazy`, like `cons-stream` in
SICP. (It doesn't memoize, so 'lazy' is a bit of a misnomer -- sorry,
I should change the name or the behavior.)

Lists are immutable, arrays are mutable. A flexarray can grow and
shrink, a plain array has fixed length.

Iterating through collections will be explained below, under the `for`
form.

Cant doesn't intend to support improper lists -- that is, anything
like classical Lisp's `(cons 'thing 'not-a-list)` producing `(thing
. not-a-list)`. At the moment you can get away with `(link 'thing
'not-a-list)` in Cant until you call a sequence method, such as
`.count`, on it. I think I want to make it an error to construct the
improper list in the first place; it's allowed for now because it can
be useful when you replace this `not-a-list` with an object that acts
like a list. That's how `link/lazy` above is implemented, in fact. For
`link` to complain about non-lists at construction time, we'll need a
way to certify new objects as legitimate lists.


## Terms, patterns, and objects

Expressions like `m.keys` and `(bag .add! key)` are syntactic sugar:

| Expression          | After reading | After parsing | Note |
| ------------------- | ------------- | ------------- | ---- |
| `m.keys`            | `(m .keys)`        | `(call m {.keys})` |   |
| `(bag .add! key)`   | `(bag .add! key)`  | `(call bag {.add! key})` |   |
| `(f x y z)`         | `(f x y z)`        | ```(call f {~ x y z})``` | (Assuming `f` is not defined as syntax.) |

The subexpression `{tag e1 e2 e3}` evaluates its subexpressions `e1`,
`e2`, `e3`, and creates a datum called a *term*. A term has a tag and
arguments. A term is data, not an object with identity; its equality
test is structural. If the arguments can be ordered, then so can the
term. Terms are distinct from all other data types.

In cant, `(call receiver message)` is a special form. It evaluates
the receiver, evaluates the message, and sends the latter to the
former.

When parsing an implicit call (that is, where the first element of a
compound expression is not the name of a special form or macro), it's
sugar for an explicit call with a term as the message expression. If
there's a dotted symbol as the second element, that symbol will be the
term's tag, as in the first two lines above; else the tag is `~`, the
convention for a tuple, as in the third line.

(TODO explain `(f @args)` too)

The `make` special form creates and returns a new object:

```
-> (make alice
     (to (~ .greet someone) `("hey" ,someone))
     (to ~.scram            "see ya"))
#<alice>
-> (alice .greet "bob")
("hey" "bob")
-> alice.scram
"see ya"
```

This definition had a name and a list of clauses. At creation time,
the name was bound to the new object. At call time, the message gets
matched against the pattern of each clause in turn until a match
succeeds. A match may bind variables (such as `someone`, above). Then
the corresponding body is evaluated in the environment created by the
match.

You can use terms and patterns in non-OO code as well. For instance,
patterns work in `let` forms and function parameters:

```
(to (c+ {complex x1 y1} {complex x2 y2})    ;; from library/complex.cant
  {complex (+ x1 x2)
           (+ y1 y2)})

(let {pq min1 rest1} pq1)                   ;; from library/pairing-heap.cant
```

The `may` form, like `make`, finds the first successful match in a
sequence of clauses and then evaluates the body of that clause:

```
(may (m .get key)
  (be #no   "Missing key")
  (be value `("The result is" ,value)))
```

The difference is that the subject being matched against is the value
of the first subexpression, `(m .get key)` here.

If in the `may` every match fails, a match error is raised.


## Kinds of patterns

The pattern syntax is hopefully obvious enough to pick up everything
common by example, but here it all is for reference:

- Just `_` matches anything.

- Any other symbol like `variable` also matches anything, and binds
  `variable` to the subject in this scope. It's a syntax error to have
  two binding instances of the same variable.

- A constant like `42` -- any constant that may also be written as an
  unquoted expression.

- A quoted constant `'whatever`.

- A term `{tag pattern1 pattern2 ...}` matches the tag literally and
  the subpatterns recursively. Similarly the array literal `[pattern1
  pattern2 ...]` matches an array subject when the subpatterns match
  the array's elements.

- `(list<- pattern1 pattern2 ...)` matches a list in the same way the
  array literal above matches an array. In general, a pattern that is
  a Lisp list like `(foo x y z ...)` is interpreted specially
  according to `foo`: other special foo's follow below.

- `(and pattern1 pattern2 ...)` matches when the subject matches all
  of the subpatterns, trying them each in left-to-right order.

- `(view expression subpattern)` does this: evaluate `expression`,
  then call the value with the subject as argument, then take the
  result as the new subject to match against `subpattern`.

The remaining kinds of pattern are syntactic sugar implemented on the
above:

- `{tag pattern1 ... @rest-pattern}` and similar for `[]` and
  `(list<-)`: matches when the subpatterns match corresponding
  subparts, but with `rest-pattern` matching zero or more remaining
  subparts. `rest-pattern` will be bound to a list, even if the
  subject is a term or an array.

- A quasiquote pattern works like a quasiquote expression, but for
  matching.

- `(? expression)` evaluates `expression`, then calls the value with
  the subject, then succeeds iff the result is `#yes`. (I suppose it
  should be an error if the result is not `#yes` or `#no`. Currently
  it can be anything.)

- `(? expression pattern)` is sugar for `(and (? expression) pattern)`.

- `(= expression)` evaluates `expression` and succeeds if the value
  equals the subject.

- `(optional pattern1 ... pattern_n)` matches a list of length 0 to n,
  where any elements that do exist match the corresponding subpattern.


## The callable-message idiom

Consider `(each ~.count rows)`. It calls `~.count` on each row of
`rows` (like Scheme `map`). But then what do we mean by `(~.count
row)`? It's sugar for `(call {.count} {~ row})`. The [behavior of
a
term](https://github.com/darius/cant/blob/master/abcs/00-primordia/types/term.cant#L2),
like `{.count}`, on receiving a message like `{~ row}`, is to call
`row` with the term as the message, as if you'd written `row.count`.

So, [for example](https://github.com/darius/cant/blob/master/library/regex-gen.cant#L42),
```
(let r-lengths (~.keys (~.range (each ~.count r-matches))))
```
could have been written
```
(let r-lengths (((each ~.count r-matches) .range) .keys))
```
but I'd find that center-embedding harder to follow, and rather un-Lispy.
(So why wasn't Cant syntax designed with the selector in 'car'
position all the time? It was at the very beginning, but in time it
just felt more right to keep the message part of the call expression
in one piece.)

The same works for [more complex
messages](https://github.com/darius/cant/blob/master/examples/automata/text-register-machine.cant#L100-L101):
`(each (~ .get 0 padding) lists)` for each list gets the first
element, or `padding` if empty. But in general you still may have to
[fall back to function
syntax](https://github.com/darius/cant/blob/master/examples/automata/turing.cant#L26):
``` (each (-> ("~w" .format it)) squares) ```


## Input and output

As we saw at the start, there's an object `out` which can write to
standard output:
```
(to (I-am name) (out .say "Hi, ~w!\n" name))
```
It's an instance of the 'sink' type. You can see its protocol in
[sink.cant](https://github.com/darius/cant/blob/master/abcs/00-primordia/types/sink.cant)
(defining primitive sinks) and
[sink-trait.cant](https://github.com/darius/cant/blob/master/abcs/00-primordia/sink-trait.cant)
(a trait to help you define new sink types).

Conversely, a primitive 'source' reads from an input file. (See
[source.cant](https://github.com/darius/cant/blob/master/abcs/00-primordia/types/source.cant).
There's no trait yet because I haven't had occasion to define new
source types.) When a source reaches end-of-file, it returns a special
object for which `(eof? x)` is true.

Standard input is named `stdin`, and yes, that's an uncanty name and
that sucks and I don't know what to call it. I'm sort of balking at
putting 'in' in the namespace.

You connect sources and sinks to named files using `open-input-file`
and `open-output-file`. Normally you use the convenience functions
`with-input-file` and `with-output-file` which close the files once
you're done -- just grep for examples in the codebase.

Input and output are our first examples of powers that require
*capabilities*. Code in a module does *not* have capabilities by
default.

(It's planned that modules will be provided something like
Javascript's 'console.log', which in strict terms is a power, but
should 'not matter' insofar as you don't delegate these debug
capabilities, because no ordinary code, including the modules
themselves, will have read access to the logs.)

'String sinks' implement the sink protocol but just produce a string
out of the characters they are given. The constructor `string-sink<-`
is in the computational setting, not a capability, because creating a
string is not an effect. (Consuming time, space, energy are not
considered to be 'effects'.) You'd typically use the convenience
function `string<-writer` which creates a string sink, calls your
writer with it, and then returns the string:
```
-> (string<-writer (on (sink) (sink .say "hel") (sink .say "lo")))
"hello"
```

There ought to be corresponding string sources, but I haven't got
around to them.


## Capabilities

`computational-setting` in a module vs. full powers in `incant` or the listener

`(cant .play expression setting)`

introspection & debugging

The runtime in `abcs/` is loaded in a primordial setting which binds a
bunch of implementation primitives (to names like `__vector-set!`)
some of which could corrupt the system if misused. These primitives
are not directly accessible to other code, but you can get to them
through the debugger (or, at the moment, just intercepting an error).
I'm not currently aspiring to finer-grained debug capabilities,
e.g. so you could give a debug capability to an execution of some of
your code to another user without exposing the whole system. Of course
that goal is a reasonable wish, and doable.


## Modules

`(use 'library-name)` -- sandboxed

`(use "filename")` -- currently not sandboxed, until I finish converting the codebase

relative `use`

import, export


## Boxes and assignment

There's a fairly awkward substitute for mutable variables, the box type:

```
-> (let c (box<- 0))
<box 0>
-> c.^
0
-> (c .^= (+ c.^ 1))
-> c.^
1
-> (c .update ~.+)    ;; ~.+ is an increment method on numbers
2
-> c.^
2
```


## Comparison

We've defined how objects are the same or not, as reported by
`=`. Some objects can be ordered, with `<`, `>`, `<=`, `>=`. Also
there's `<=>` meaning both `<=` and `>=`. These functions work by
calling the `.compare` method: `(< a b)` means that `(a .compare b)`
is `-1`. An error is raised if the result is not that or `0` or `1`.

Primitive data like numbers, lists, and terms define `.compare` in the
usual way, requiring compatibly-typed arguments. They complain if
asked to compare, e.g., `1` to `x`.


## Numbers

Non-integer number types aren't really supported, except by accident
insofar as Chez Scheme primitives make them work. The reader doesn't
know about floats, for instance. This needs filling in relatively
early, compared to other features still on the wishlist.


## Abstract data types, synergy, trademarks

Don't exist yet. Only the vaguest notions. This needs to be addressed
for users to be able to define new types of *data* -- as we've seen,
an object you can make with `make` differs in an essential way from a
datum like a term, behaving differently under `=`. Lists are data, but
object-like too, and so they had to be primitive. We might like to be
able to define new kinds of numbers as well, interoperating with the
primitive ones.

(I guess you can fake it well enough already for some purposes using
`=` on capabilities, but we'll need language support to make the above
wishlist practical.)


## Traits

We've seen how to define objects, but nothing like inheritance. In
place of inheritance, Cant objects can delegate messages to other
objects. If you want open recursion, the delegatee must receive the
original receiving object along with the message. You could make this
happen by hand like

```
(to (delegatee receiver message)
  (may message
    (be ~.bump receiver.thump)
    (else (miranda-trait receiver message))))   ; Explained below.

(make delegator
  (to ~.thump "Ka-THUMP")
  (to message (delegatee delegator message)))

delegator.bump  ;; => "Ka-THUMP"
```

The `miranda-trait` is a presupplied delegatee-of-last-resort which
implements default responses to standard messages -- it's how, for
instance, you get a default visible representation for an object like
this:
```
-> (make example)
#<example>
```
Here `example` implicitly ends with a `to` clause like `(to message
(miranda-trait example message))`. When the listener tries to print
this example object, it does a call like `(example .selfie out)`
which ends up handled by a primitive method in `miranda-trait`.

There's some moderately helpful syntax we could've used to define
`delegatee` and `delegator` above:
```
(make-trait delegatee receiver
  (to ~.bump receiver.thump)
  (to message (miranda-trait receiver message)))

(make delegator {extending delegatee}
  (to ~.thump "Ka-THUMP"))
```

This seems the most simpleminded way to provide open recursion, where
wanted, without making it ubiquitous where not. The syntactic sugar
for it has not been considered much so far, either. We might change
all this to a 'real' trait system in the future, perhaps like the
traits of newer Smalltalks.

Traits are used in the codebase primarily for the collections
hierarchy, in
[abcs/00-primordia/](https://github.com/darius/cant/blob/master/abcs/00-primordia/).
They come up too in factoring common behavior out of variable-arity
primitives:
[runtime.cant](https://github.com/darius/cant/blob/master/abcs/00-primordia/runtime.cant#L269-L302)
(and elsewhere). There's also a small example of a game in OO style,
[examples/games/wumpus.cant](https://github.com/darius/cant/blob/master/examples/games/wumpus.cant).
(I wouldn't recommend making a habit of creating stateful traits as
that example does, but that's how it worked out.)


## More syntax: `for`

The `for` form is syntactic sugar primarily for iterating over
sequences, though it has other uses as well. For instance, `(for each
((x xs)) (foo x))` is equivalent to `(each (on (x) (foo x)) xs)`
which is mostly equivalent to `(each foo xs)`, which is Cant's name
for Scheme's `(map foo xs)`.

The `for` form, `(for fn ((x e) ...) body ...)`, just rearranges its
parts in the same way that Scheme's `let` rearranges `(let ((x e) ...)
body ...)`, but with `fn` stuck in front.

Some functions useful with `for`:

`(each f '(a b c))` = `(list<- (f 'a) (f 'b) (f 'c))`. You can also do
`(each f xs ys)` which works similarly but calling `(f x y)` for each
corresponding `x` and `y`. (It's an error if the lists are different
lengths.) And so on for any number of lists. Many of the functions
below also work in this variable-arity way.

`(each! f xs)` calls `(f x)` for each `x` in `xs`, in order, for the
sake of any side effects.

`(gather f '(a b c))` = `(chain (f 'a) (f 'b) (f 'c))`

`(those ~.even? '(3 1 4 1 5 9 2 6))` = `'(4 2 6)`

`(some pass? xs)` = does `pass?` approve any `x` in `xs`?
More precisely, the first non-`#no` result of `(pass? x)` for `x` in `xs`,
or else `#no`. Check the xs in order, short-circuited.

`(every pass? xs)` = does `pass?` approve every `x` in `xs`?
More precisely, if `xs` is empty then `#yes`, else `#no` if for some
`x` in `xs` `(pass? x) is `#no`, else `#yes`. (Perhaps we should
define that last case as `(pass? xs.last)` instead.) In order,
short-circuited.

`(yeahs maybe xs)` = a list of the non-`#no` results of `(maybe x)` for `x` in `xs`,
in order.

`(where pass? map)` = a list of the keys of `map` whose corresponding
values are approved by `pass?`.

`(foldr f '(a b c) z)` = `(f 'a (f 'b (f 'c z)))`

`(foldl f z '(a b c))` = `(f (f (f z 'a) 'b) 'c)`

The meaning of `for` with the fold functions is initially less
obvious, but soon becomes familiar, e.g.
```
(for foldl ((state initial-state) (input inputs))
  <compute the next state from the current state and input>)
;; => final state
```

Like `foldr` but requiring `xs` to be nonempty:
`(foldr1 f '(a b c))` = `(f 'a (f 'b 'c))`

`(sum-by f '(a b))` = `(+ (f 'a) (f 'b))`

`(tally-by f '(a b))` = `(+ (~.count (f 'a)) (~.count (f 'b)))`

`(max-by f xs)` = an `x` in `xs` giving the maximum value of `(f x)`.

`(min-by f xs)` = similar

`(mayhap f ?thing)` is `(f ?thing)` unless `?thing` is `#no`,
preserving no-ness.

There are lazy-list versions of many of these, with names like
`each/lazy`.


## Efficiency of sequences

The previous section described many functions on lists. Those actually
all work on generic sequences: most or all of them (TODO any
exceptions?) traverse their sequence arguments using the
`.first`/`.rest`/`.none?`/`.some?` methods. These methods are presumed
to be an efficient-enough way to walk the sequence, which assumption
can be wrong: for instance, for an array or a string, `.rest` returns
another array or string, implying a quadratic cost of traversal.

Wouldn't it be better to just change the performance profile of these
`.rest` methods? There are a couple ways that could go: a rope-style
representation (at least for the *immutable* sequences), or a
reference-into-a-shared-array one. The former is clean and
robustly-performing but with greater complexity and overhead; the
latter has a harder-to-think-about performance profile (a small peek
into a big array shielding it from GC, with this phenomenon then
tempting us towards fancier tricks in the core implementation to make
the problem come up less). I think in Cant we want the core to be able
to be implemented in a simple pretty-efficient way, letting you get
fancier in your own code where you need it -- granted that the current
implementation is not at all efficient.

Instead, we follow a convention: a collection's `.values` should be
*efficient to walk through* using `.first`/`.rest`. So when you're
invoking the above traversals, it's sensible to either yourself call
`.values` on the sequence argument, or know that it's short enough
that you don't care. (For collections in general, the `.values` method
returns a sequence of the values from its key/value pairs. When the
collection is a sequence, `.values` should represent the same
sequence.)

This business of calling `.values` yourself is definitely a wart. I
can see two ways to improve on it: make the language or the library
call `.values` for you implicitly, or change the common traversals
from being defined as functions on sequences to being methods on
collections, defined in the collections traits, with potentially
specialized implementations.

I suppose we'll end up doing something like one of those, but the
current simpleminded design has been holding up well enough to go on
with. I'm unhappy with the idea of making `.values` implicit because I
don't like too much going on under the hood for very-common
operations. The functions-to-methods change would need some kind of
syntax change to take the place of the current trivially-simple `for`
macro, and would mean relying even more on traits, which are OO in
principle but stand in some tension with the principle of least
authority.

Another weakness of the design of traversals is the tendency to
eagerness. The sequence you pass in needn't be materialized: it can be
a lazy list, or another incrementally-produced representation such as
an interval. So in principle there may be no need for iterators, a
whole separate kind of thing in the ontology of many other
languages. However, Cant doesn't make it as convenient to create a
lazy sequence as an eager one, or as natural to remember that you may
be consuming a lazy sequence which may have side effects. With
iterators, these issues would be abated. (I was aware of this design
problem when I chose to make sequence traversals just work directly on
sequences. This sort of pattern has been common in this project: "Ugh,
I know this has problems, but so far as it goes it seems simpler than
the alternatives; maybe living with it for a while will make it clear
what to do" -- trying to learn from the simplest thing that could
possibly work, acknowledging that often it won't. A better designer
could work out a lot more issues more in advance.)


## More list functions

`(sum xs)` just adds up `xs`.

`(tally xs)` sums `x.count` for each `x`.

`(zip xs ys zs ...)` = a list of `(~ x y z ...)` for each `x` in `xs`
and positionally-corresponding `y` in `ys`, etc. The inputs must all
have the same length.

`(grid* xs ys)` = a list of lists, pairing each x in xs with every y
in ys. (It's concerning that Cant has multiple reasonable ways to
represent a pair: tuples, lists, arrays...)


## More syntax: `given`

To create an anonymous function with multiple pattern-action clauses:
```
(given
  (be pattern1 action1)
  (be pattern2 action2)
  (else action3))
```

The use for it doesn't seem to come up often. Here's one good example:
the [counting function in
examples/lambda-calculus/prelude.cant](https://github.com/darius/cant/blob/master/examples/lambda-calculus/prelude.cant#L15-L18). Without
`given` the most obvious way to write it would be the stuttery:
```
{primitive (on (n)
             (begin counting ((n n))
               (may n
                 (be 0          lc-zero)
                 (be (? count?) (apply lc-succ (counting n.-))))))}
```


## More idioms

The
[function](https://github.com/darius/cant/blob/master/abcs/30-functions.cant#L226)
`(hey focus action1 action2 ...)` returns its first argument, `focus`,
after sending it to each of the actions. [For
example](https://github.com/darius/cant/blob/master/library/sturm.cant#L192-L193),
```
(hey stdin.read-char
     (-> (surely (not (eof? it)))))
```
returns the next character from standard input, but raises an
assertion error if it's the end-of-file. (I know, using an assertion
here was lazy and sinful.) Perhaps more commonly you'd use `hey` to
create a new object (the focus) and initialize it (the actions).

The
[function](https://github.com/darius/cant/blob/master/abcs/30-functions.cant#L220)
`(take input f g h)` is like `(h (g (f input)))`. It's [occasionally
handy](https://github.com/darius/cant/blob/master/library/bag.cant#L25-L27)
in place of things like Clojure's threading macros.


## Exceptions, ejectors

XXX


## Introspection

XXX


## Miscellany

Now that we've glanced over the greater part of the core, you might
find it worthwhile to review [the metacircular
interpreter](https://github.com/darius/cant/blob/master/examples/kernel.cant)
to see if it makes sense. You can see it getting the smoke test in
[test/test-kernel.cant](https://github.com/darius/cant/blob/master/test/test-kernel.cant).

```
more std types
  term
  void
  meta-stuff
```

more naming conventions:
foo<-
dest<-src
foo-by

arg conventions to work nicely with `for`

stdlib:
surely 
parson, simple-parser (need to extract it)
string .format
squickcheck

load, repl, debug stuff, command line

read syntax: [] {} @ ...?

cheat sheet, like https://github.com/jeapostrophe/racket-cheat/blob/master/racket-cheat.scrbl
cant-mode.el


## Infelicities

problems about the syntax

list of biggest lacunae

request for ideas:
- collections read syntax
- module refs without listing them all at import
- better support for mutables?
