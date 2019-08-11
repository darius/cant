## What's this for?

Call it outsider art. I've long wanted a computing system that's
completely explained, simple enough to grasp in full detail without
getting bored or overwhelmed, and powerful enough that I actually want
to use it. Others have done this well enough now -- particularly
Wirth's Oberon system -- that making another from scratch becomes hard
to justify as a project offering reasonable reward to effort. But I
still want one.

Such a system needs a high-level programming language. I'm trying to
work one out to fit in this fantasized Robinson Crusoe environment.
Scheme is another inspiring model; you could definitely use it as is. 
But, you get the drift: I started over anyway.

This FantasyOS ought to support capability security, so this language
should too (though it doesn't quite yet). My previous go at a
capability-secure Scheme dialect, consp, persuaded me it's important
that "all you can do is send a message" (or almost all). That is,
Scheme has plenty of data types, plus different operations on each
type, and the operations insist you pass them an object of just the
type they want, never some wrapper you just made up. In Squeam, most
operations are like function calls instead; you send the object a
message, it decides what to do with it, and you're mostly free to
substitute your own objects that interpret the message their own way.


## What else changed from Scheme?

Collections follow a common protocol. Every collection is a kind of
map. (Lists are sequences, which are maps from a natural-number index
to a value (plus some extra methods).)

But Squeam is not class-based or prototype-based; the expression to
make an object is more like a Scheme lambda-expression. It's sort of
like the message-passing objects in SICP chapter 3, but streamlined,
with pattern-matching. (See the E programming language for the actual
inspiration.) Instead of inheritance there are 'traits'.

Patterns are supported everywhere you can bind a variable. Definitions
work the same at top level as in nested scopes. The scope of a binding
is the whole sub-block it's in -- recursion and forward references
aren't special. (A use-before-define error can happen at runtime.)
Variables are immutable. (There's a 'box' type for when you need a
mutable variable; also, by special dispensation, definitions at the
listener can update a binding.)

Lists and strings are immutable too. Currently the built-in hashmaps,
arrays, etc. are mutable, though I think now they shouldn't be.

Scheme has lots of undefined behavior; in Squeam the ideal is for any
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

TODO also mention hashing and ordered comparison

The remaining differences are a whole lot of bikeshedding. For
instance, I'm trying to use only ordinary English words, preferably
short, and never abbrevs like `cons`, jargon like `lambda`, or names
like `Boolean`. I could offer the excuse that this standard jibes with
the goal of an explainable system -- but I went way overboard in
changing familiar names. The only real justification for weighing
familiarity to *programmers* at *zero* is that it pleases me and I
want to see where it goes.


## OK, but can't you list some vague aspirations and utopian priorities?

I want hacking in it to make me happy, so that I'm itching to get back
to the computer. What makes me happy?

* Clear understanding.

* Pretty code.

* Rapid feedback.

* Simple design -- feeling like a know-it-all.

* Code that works the first time.
  (This is in tension with "rapid feedback", yeah.)

* Feeling like an augmented intellect. Getting out seemingly more than
  I put in.

What makes me unhappy?

* Filling out forms. 

* Needing permission.

* Unpleasant surprises.

* Feeling puzzled without good reason.

Priorities. Squeam should:

1. Help you to understand the behavior of programs. This might be for
explorable explanations, or for debugging and development. Understanding can
include what changes when you change the code.

Thus:

* Determinism. (Cf. the importance of replication in science.)

* Time travel.

* Easy prettyprinting of everything.

* Live update of code.

* Halp.

* (what else?)

2. Be itself understandable, as an explorable explanation of
computing.

3. Run some useful programs well. This is only the third priority;
most systems don't elevate the first two priorities above it. OTOH
there are educational systems like Little Smalltalk (a good one!)
which I'm not sure have ever run a program for actual use.


## The implementation

It's sort of Smalltalky, where for now the interpreter internals
always appear as meta-objects with no optimization, and even primitive
objects execute via high-level scripts (in `abcs/runtime.scm`). This is
motivated by the goal of a self-sustaining whole system eventually
supporting powerful debugging. To not be incredibly slow, the design
will need to get fancier.

The interpreter and core library, at this writing, amount to ~4000
lines of Scheme and Squeam. Back in the 90s I used a bytecode Scheme
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

Be in this directory (the one holding the file `squeam.scm`) and then:

```
$ ./squeam.scm
-> (to (I-am name) (format "Hi, ~w!\n" name))
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
[eg/](https://github.com/darius/squeam/tree/master/eg) and
[library/](https://github.com/darius/squeam/tree/master/library). To run an
example (
[FizzBuzz](https://github.com/darius/squeam/blob/master/eg/fizzbuzz.scm)
here):

```
$ ./squeam.scm eg/fizzbuzz.scm
```

Or load it in the listener:

```
$ ./squeam.scm
-> (load "eg/fizzbuzz.scm")
1
2
Fizz
...

```

The global environment at startup is populated from
[abcs/](https://github.com/darius/squeam/tree/master/abcs) (plus a
handful of modules from
[library/](https://github.com/darius/squeam/tree/master/library)). Not
that these make great example code necessarily, but they're the first
places to look to clear up questions you may have.

The underlying interpreter in Scheme lives in
[player/](https://github.com/darius/squeam/tree/master/player).


## The bikeshedding

Let's start with the equivalents of familiar Scheme syntax:

| Scheme                        | Squeam        | Note          |
| ----------------------------- | ------------- | ------------- |
| `(define x 42)`                 | `(let x 42)`       |  Returns 42 as the value. Definitions are expressions.  |
| `(define (f x) e)`              | `(to (f x) e)`     |   |
| `(define (f x) (lambda (y) x))` | `(to ((f x) y) x)` |   |
| `(lambda (x y) e)`              | `(given (x y) e)`  |   |
| `(begin a b c)`                 | `(do a b c)`       |   |
| `(let () a b c)`                | `(hide a b c)`     |   |
| `(if t p q)`                    | `(if t p q)`       | `p` and `q` are in nested scopes, as if in `hide` blocks. |
| `(and x y)`                     | `(and x y)`        | Similarly, `y` is in a nested scope. |
| `(or x y)`                      | `(or x y)`         | Ditto. |
| `(if p (begin a b))`            | `(when p a b)`     | `a` and `b` are in their own common nested scope.  |
| `(if (not p) (begin a b))`      | `(unless p a b)`   |   |
| `(cond (p a) (else b))`         | `(case (p a) (else b))`  |  `case` isn't the greatest name. Suggestions? |
| `#t`                            | `#yes`  |   |
| `#f`                            | `#no`  |   |
| `(let looping ((v init)) body)` | `(begin looping ((v init)) body)` |  The 'ing' is a convention. |

Scheme functions on lists:

| Scheme                        | Squeam        | Note          |
| ----------------------------- | ------------- | ------------- |
| `(cons x xs)`                 | `(link x xs)`       |  |
| `(append xs ys)`              | `(chain xs ys)`     |  |
| `(list x y z)`                | `(list<- x y z)`    | Though it's more common to use quasiquoting. |
| `(null? xs)`                  | `(null? xs)`, `xs.none?`, or `(xs .none?)`     | `xs.none?` is reader sugar for the last expression. All collections answer this message, though not all objects. `null?` would be useful when you don't know if the argument is a collection. |
| `(pair? x)`                   | `(link? x)`      |  |
| `(list? x)`                   | `(list? x)`     |  Squeam doesn't plan to support improper lists, though I haven't got around to making them an error. |
| `(car xs)`                    | `xs.first` or etc.     |  |
| `(cdr xs)`                    | `xs.rest`     |  |
| `(length xs)`                 | `xs.count`     |  |
| `(list-ref xs n)`             | `(xs n)`     |  |
| `(list-tail xs n)`            | `(xs .slice n)`     |  There's also `(xs .slice start-index after-index)` |
| `(member x xs)`               | `(xs .slice (xs .find x))`     |  (Provided `x` is in `xs`. More on `.find` below.) |
| `(cadr (assoc 'x '((a b) (x y))))`   | `((map<- '((a b) (x y))) 'x)`     |  More on `map<-` below. |

The accessors on lists above are all generic. They apply to strings too, for a start:

| Scheme                        | Squeam        | Note          |
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

| Scheme                        | Squeam        | Note          |
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
| `m.items`                     | A sequence of key-value pairs. (The name `items` is from Python; any ideas for a better name? `mappings`?) |
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

You can create a mutable hash-map with `(map<-)` (initially empty), `(map<-
association-list)`, or `(export name1 name2 ...)` which is like
```(map<- `((name1 ,name1) (name2 ,name2) ...))```.

(Warning: the current implementation in Chez Scheme can't hash
consistently with the equality test, in general. You're safe using
keys that are purely data (such as a link-list all of whose elements
are pure data as well, and so on recursively). A mutable object or
e.g. a list of mutable objects is not guaranteed in this interim
implementation to work. This is because Chez Scheme doesn't offer a
way to make a hashtable keying on a mix of identity (`eq?`) hashing
and a user-defined equality predicate. A mythical future production
Squeam system needs to define its hashmaps primitively.)

A bag is a kind of mutable map whose values are all counts. (Maybe we
should support negative values too, like Python's `Counter`?) For a
key that's absent from the bag's explicit set of keys, `(bag key)` is
0 rather than an error. `(bag .add! key)` increments the count for
`key`. There are some more convenience methods I'll document later.

(I'll need to revisit all this when we make collections immutable.)

A set is a kind of bag whose nonzero counts are 1. This is another
case of this design prioritizing substitutability over specializing
every subtype's interface.

A list or sequence is a map whose keys are natural numbers. (Squeam
calls a natural number a 'count', in its ridiculous stamp-out-jargon
reform campaign.) There are a few extra operations like chaining two
sequences.

You can make lazy sequences with `link/lazy`, like `cons-stream` in
SICP. (It doesn't memoize, so 'lazy' is a bit of a misnomer -- sorry,
I should change the name or the behavior.)

Lists are immutable, arrays are mutable. A flexarray can grow and
shrink, a plain array has fixed length.

The `for` form is syntactic sugar primarily for iterating over
sequences, though it has other uses as well. For instance, `(for each
((x xs)) (foo x))` is equivalent to `(each (given (x) (foo x)) xs)`
which is mostly equivalent to `(each foo xs)`, which is Squeam's name
for Scheme's `(map foo xs)`.

The `for` form, `(for fn ((x e) ...) body ...)`, just rearranges its
parts in the same way that Scheme's `let` rearranges `(let ((x e) ...)
body ...)`, but with `fn` stuck in front.

Some functions useful with `for`:
```
each each! those gather yeahs every some zip foldr foldl where tally 
```


## Terms, patterns, and objects

Expressions like `m.keys` and `(bag .add! key)` are syntactic sugar:

| Expression          | After reading | After parsing | Note |
| ------------------- | ------------- | ------------- | ---- |
| `m.keys`            | `(m .keys)`        | `(call m {.keys})` |   |
| `(bag .add! key)`   | `(bag .add! key)`  | `(call bag {.add! key})` |   |
| `(f x y z)`         | `(f x y z)`        | ```(call f `(,x ,y ,z))``` | (Assuming `f` is not defined as syntax.) |

The subexpression `{tag e1 e2 e3}` evaluates its subexpressions `e1`,
`e2`, `e3`, and creates a datum called a *term*. A term has a tag and
arguments. A term is data, not an object with identity; its equality
test is structural. If the arguments can be ordered, then so can the
term. Terms are distinct from all other data types.

In squeam, `(call receiver message)` is a special form. It evaluates
the receiver, evaluates the message, and sends the latter to the
former.

When parsing an implicit call (that is, where the first element of a
compound expression is not the name of a special form or macro), if
there's a dotted symbol as the second element, it's parsed as a call
with a term as the message, as in the first two lines above; else the
message is a list, as in the third line.

The `make` special form creates and returns a new object:

```
-> (make alice
     (to (_ .greet someone) `("hey" ,someone))
     (to _.scram            "see ya"))
#<alice>
-> (alice .greet "bob")
("hey" "bob")
-> alice.scram
"see ya"
```

This definition had a name and a list of clauses. At creation time,
the name was bound to the new object. At call time, the message gets
matched against the pattern of each clause in turn until a match
succeeds. A match may bind variables (such as `someone`). Then the
corresponding body is evaluated in the environment created by the
match.

You can use terms and patterns in non-OO code as well. For instance,
patterns work in `let` forms and function parameters:

```
(to (c+ {complex x1 y1} {complex x2 y2})    ;; from lib/complex.scm
  {complex (+ x1 x2)
           (+ y1 y2)})

(let {pq min1 rest1} pq1)                   ;; from lib/pairing-heap.scm
```

The `match` form, like `make`, finds the first successful match in a
sequence of clauses and then evaluates the body of that clause:

```
(match (m .get key)
  (#no   "Missing key")
  (value `("The result is" ,value)))
```

The difference is that the subject being matched against is the value
of the first subexpression, `(m .get key)` here.

TODO more about patterns


## Miscellany

```
more std types
  source, sink, string-sink, eof
  box
  term
  void
  meta-stuff
```

import export

=, not=, compare

traits
miranda methods

evaluate, call

more naming conventions:
foo<-
dest<-src
foo-by

arg conventions to work nicely with `for`

stdlib:
surely 
parson, simple-parser (need to extract it)
format
squickcheck

load, repl, debug stuff, command line, `use`

read syntax: [] {} @ ...?
no (a . b)

exceptions, ejectors

cheat sheet, like https://github.com/jeapostrophe/racket-cheat/blob/master/racket-cheat.scrbl
squeam-mode.el


## Infelicities

problems about the syntax

list of biggest lacunae

request for ideas:
- collections read syntax
- module refs without listing them all at import
- better support for mutables?

