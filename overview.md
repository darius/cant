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
objects execute via high-level scripts (in `lib/runtime.scm`). This is
motivated by the goal of a self-sustaining whole system eventually
supporting powerful debugging. To not be incredibly slow, the design
will need to get fancier.

The interpreter and core library, at this writing, amount to ~2500
lines of Scheme and Squeam. Back in the 90s I used a bytecode Scheme
of my own at ~5000 lines of C and Scheme, and that size would not be
too crazy a goal for this. Over 10k lines would be disappointing.

It seems hard to find systems anywhere near as small as the
educational toys but with realistic support for real use requiring
debugging, etc. I want to make that system or learn why I can't. (But
'real' here does not include interoperating with other,
already-complex systems.)

There's plenty of unfinished crap in this repo, not clearly marked.


## Quick start for Schemers

```
$ squeam
sqm> (to (I-am name) (format "Hi, ~w!\n" name))
#<I-am>
sqm> (I-am 'Alice)
Hi, Alice!
sqm> 
```

The less-happy path:

```
sqm> (I-am 'but 'human)
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
sqm> (debug)
Enter ? for help.
debug> ?
? help      - this message
q quit      - quit the debugger
r resume    - continue from here, with the value of an expression
u up        - up to caller
d down      - down to callee
e env       - enumerate the variables in the current environment
v value     - evaluate an expression in the current environment
b backtrace - show all of the stack up from here
debug> q
ok
```


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
| `(null? xs)`                  | `(null? xs)`, `xs.empty?`, or `(xs .empty?)`     | `xs.empty?` is reader sugar for the last expression. All collections answer this message, though not all objects. `null?` would be useful when you don't know if the argument is a collection. |
| `(pair? x)`                   | `(link? x)`      |  |
| `(list? x)`                   | `(list? x)`     |  Squeam doesn't plan to support improper lists, though I haven't got around to making them an error. |
| `(car xs)`                    | `xs.first` or etc.     |  |
| `(cdr xs)`                    | `xs.rest`     |  |
| `(length xs)`                 | `xs.count`     |  |
| `(list-ref xs n)`             | `(xs n)`     |  |
| `(list-tail xs n)`            | `(xs .slice n)`     |  There's also `(xs .slice start-index after-index)` |
| `(member x xs)`               | `(xs .slice (xs .find x))`     |  (Provided `x` is in `xs`. More on `.find` below, TODO) |
| `(cadr (assoc 'x '((a b) (x y))))`   | `((map<- '((a b) (x y))) 'x)`     |  |

The accessors on lists above are all generic. They apply to arrays too, for a start:

| Scheme                        | Squeam        | Note          |
| ----------------------------- | ------------- | ------------- |
| `(vector a b c)`              | `(array<- a b c)`       |  |
| `(make-vector n init)`        | `(array<-count n init)`     |  |
| `(vector-length v)`           | `v.count`     |  Same as above. |
| `(vector-ref v n)`            | `(v n)`     | Ditto. |
| `(vector-set! v n x)`         | `(v .set! n x)`     | There's nothing like Common Lisp setf, so far. |
| `(list->vector xs)`           | `(array<-list xs)`       |  |
| `(vector->list v)`            | `v.values`       |  |
| `(vector? x)`                 | `(array? x)`       |  |

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
              -- we don't have general multidimensional arrays yet.
```

```
each each! those gather filter where tally every some zip foldr foldl
delayed seqs too
```

```
(for foo (...) ...)
```

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

```
term data and match: (match e (p e) ...)
message passing and make
```

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

list of biggest lacunae

request for ideas:
- collections read syntax
- module refs without listing them all at import
- better support for mutables?

cheat sheet, like https://github.com/jeapostrophe/racket-cheat/blob/master/racket-cheat.scrbl
squeam-mode.el
