# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What is Cant?

Cant is a message-oriented language descended from E and Scheme. Implementation runs on Chez Scheme. The core idea: almost everything is sending messages to objects.

## Prerequisites

Requires [Chez Scheme](https://cisco.github.io/ChezScheme/) installed.

## Running

```bash
./incant                              # REPL
./incant file.cant                    # Run file
./incant -i file.cant                 # Run file, then REPL
./run-tests                           # All tests
./t quick                             # Single test (test/test-quick.cant)
./bless-tests                         # Update expected output
```

From the listener:
```cant
(load "examples/automata/game-of-life.cant")   ;; load and run file
```

## Critical Pitfalls

### 1. Math is methods, not functions
```cant
25.sqrt              ;; NOT (sqrt 25)
(-5).magnitude       ;; NOT (abs -5) -- note: .magnitude not .abs
3.14.floor           ;; NOT (floor 3.14)
(2 .** 10)           ;; exponentiation, => 1024
```

### 2. `x.method` sugar only works for identifiers
```cant
foo.count            ;; OK - sugar for (foo .count)
(foo bar).count      ;; FAILS - expression, not identifier
((foo bar) .count)   ;; OK - explicit parens needed
(sort xs).count      ;; FAILS
((sort xs) .count)   ;; OK
```
This is a frequent source of errors. When in doubt, use explicit `(expr .method)` form.

### 3. Many names are globally bound
Can't shadow: `sum`, `count`, `each`, `keep`, `fold`, `max`, `min`, `sort-by`, `flexarray<-`, etc. Error: "An interactive setting may not shadow its parent"

### 4. Terms: use `.parts` not `.arguments`
```cant
(let t {point 3 4})
t.tag                ;; => point
t.parts              ;; => (3 4)
(t .part 0)          ;; => 3
```

### 5. Format strings: `~` is special
`~w` = write, `~d` = display. Use `~~` for literal tilde. This matters when documenting `~.method` syntax.

### 6. Library modules often use bare terms as data
```cant
;; Complex numbers - no complex<- constructor
{complex 3 4}                    ;; just use the term directly
(c+ {complex 3 4} {complex 1 1}) ;; functions operate on terms

;; Ratios similarly
{ratio 1 2}
```

## Quick Reference

### Collections
```cant
;; Lists (immutable)
'(1 2 3)             xs.count  xs.first  xs.rest  (xs 2)
(link 0 xs)          ;; cons
(chain xs ys)        ;; append
(xs .from 2)         ;; drop 2
(xs .from 1 4)       ;; slice [1,4)

;; Arrays (mutable, fixed size)
(array<- 'a 'b 'c)   (arr 0)  (arr .set! 1 'x)

;; Maps
(map<- {~ 'a 1} {~ 'b 2})     ;; immutable
(!map<-)                      ;; mutable empty
(m .set! key val)  (m .get key)  (m .get key default)
m.keys  m.values  m.items  (m .maps? key)

;; Sets (mutable despite no !)
(set<- 1 2 3)        (s .add! x)  (s .maps? x)
```

### Iteration
```cant
(each f xs)                        ;; map
(each! f xs)                       ;; map for side effects
(keep pred xs)                     ;; filter
(fold f xs init)                   ;; right fold
(amass f init xs)                  ;; left fold
(some pred xs)                     ;; any match?
(every pred xs)                    ;; all match?

;; for sugar - rearranges arguments
(for each [(x xs)] (* x x))        ;; = (each (on (x) (* x x)) xs)
(for keep [(x xs)] x.even?)
(for amass [(acc 0) (x xs)] (+ acc x))
```

### Ranges
```cant
(0 .till 5)          ;; 0,1,2,3,4 (lazy)
(1 .thru 5)          ;; 1,2,3,4,5
(0 .till 10 2)       ;; 0,2,4,6,8 (stride)
(5 .down-thru 1)     ;; 5,4,3,2,1
```

### Numbers
```cant
n.sqrt  n.magnitude  n.floor  n.ceiling  n.round
n.even?  n.odd?  n.positive?  n.negative?
(n .** k)            ;; exponent
(n .quotient d)  (n .modulo d)  (n .remainder d)
n.+1  n.-1           ;; increment/decrement
```

### Strings (texts)
```cant
s.count  s.first  s.rest  (s n)  (s .from i j)
(chain s1 s2)
("~d + ~d = ~w" .format a b (+ a b))
s.uppercase  s.lowercase  s.trim  s.capitalize
(s .split ",")           ;; split on separator
(sep .join list)         ;; join with separator
```

### Pattern Matching
```cant
(may value
  (be {point x y} (use x and y...))
  (be {empty} ...)
  (be (? number?) ...)           ;; predicate guard
  (be `(,a ,b) ...)              ;; quasiquote pattern
  (else ...))
```

### Objects
```cant
(make name
  (to ~.method-no-args body)
  (to (~ .method arg) body)
  (to (~ regular-call arg) body)
  (to message (delegate ...)))   ;; catch-all

;; With trait
(make name {extend some-trait}
  (to ~.method body))

;; Mutable state via boxes
(let x (box<- 0))
x.^                  ;; read
(x .^= 5)            ;; write
x.+1!                ;; increment, returns new value
```

### Modules
```cant
(let m (use 'library-name))      ;; from library/
(let m (use "relative/path"))    ;; .cant extension added
(import m foo bar baz)           ;; bind names locally
(export foo bar baz)             ;; at end of module
```

## Where to Find Method Definitions

| Type | Definition file |
|------|-----------------|
| Numbers | `abcs/00-primordia/types/number.cant` |
| Texts/Strings | `abcs/00-primordia/types/text.cant` |
| Lists | `abcs/00-primordia/types/link.cant` + `list-trait.cant` |
| Arrays | `abcs/00-primordia/types/array.cant` + `array-trait.cant` |
| Maps | `abcs/00-primordia/types/map.cant` + `map-trait.cant` |
| Terms | `abcs/00-primordia/types/term.cant` |
| Boxes | `abcs/00-primordia/types/box.cant` |
| Symbols | `abcs/00-primordia/types/symbol.cant` |
| I/O | `abcs/00-primordia/types/sink.cant`, `source.cant` |

Global functions: `abcs/30-functions.cant`

## Architecture

```
player/          Scheme interpreter core
  player.scm       Main interpreter
  parse.scm        Syntactic sugar / desugaring
  read.scm         Reader

abcs/            Bootstrap (loaded in numbered order)
  00-primordia/    Type behaviors and traits
    types/           Primitive type scripts
  20-cant.cant     Metaobject (parse, eval)
  30-functions.cant  Standard library functions
  50-top.cant      REPL, error handling

library/         Optional modules (use 'name)
examples/        Example programs
test/            Tests (test/test-*.cant)
```

### Message dispatch

When you call `(obj .method arg)`, this sends message `{.method arg}` (a term) to `obj`. Dispatch depends on the receiver type:

- **Primitives** (numbers, strings, lists, etc.): Dispatch is in Scheme (`player/player.scm`), delegating to Cant scripts in `abcs/00-primordia/types/` for most behavior
- **Objects** (created via `make`): Match message against the object's clauses
- **Callable values** (functions, terms): Apply with the arguments

## Known Bugs

- `ejector-protect` crashes when an ejector fires through it (marked "TODO untested" in `player/player.scm:218`)

## Additional Documentation

- `scratch/REFERENCE.md` - Comprehensive method reference for all types
- `scratch/NOTES.md` - Design observations, pitfalls, ideas
- `scratch/*.cant` - Working examples demonstrating various features
- `guide.md` - Original language guide (long, detailed)

## Emacs

`plug-ins/cant-mode.el` for syntax highlighting.
