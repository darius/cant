# Claude's Cant Exploration Notes

Observations, surprises, pitfalls, and ideas from exploring the language.

## Surprises / Pitfalls

### 1. Format string ~ escape
`~` is the format directive character (like Common Lisp). To write a literal `~`, use `~~`. This trips you up when trying to document `~.foo` method syntax in output.

### 2. Term introspection is `.parts` not `.arguments`
The guide says "You can of course trivially get the arguments of a term as a list" but doesn't say how. It's `.parts` (and `.part n` for indexing). Found by reading `abcs/00-primordia/types/term.cant`.

### 3. Void swallows output
Methods returning void (like `.reset!`) show nothing in the listener, which can look like `-> ->`. Not a bug, but initially confusing.

### 4. Math operations are methods, not functions
`sqrt`, `abs`, `floor`, `ceiling`, `exp`, `ln` are all methods on numbers:
- `25.sqrt` not `(sqrt 25)`
- `(-5).magnitude` not `(abs -5)` (note: `.magnitude` not `.abs`!)
- `3.14.floor` not `(floor 3.14)`

This is consistent with the "message passing" philosophy but surprising if you expect standard math functions.

### 5. x.method sugar only works for identifiers
`foo.bar` is sugar for `(foo .bar)`, but `(+ 1 2).bar` does NOT work.
Must write `((+ 1 2) .bar)` with explicit parens.

### 6. Many common names are taken
Can't use `sum` as a variable name - it's a function. Same for `count`, `max`, `min`, `each`, `keep`, `fold`, `sort-by`, `flexarray<-`, etc. The error message is decent: "An interactive setting may not shadow its parent".

### 7. Library modules may not have constructors
The complex number library has no `complex<-` - you just use the term `{complex 3 4}` directly. Same for ratios: `{ratio 1 2}`. This is consistent with Cant's term-oriented design but surprising if you expect OO-style constructors.

### 8. Sets use `set<-` not `!set<-`
Despite mutable maps being `!map<-`, mutable sets are just `set<-`. Inconsistent naming.

### 9. Global vs library confusion
Some things are global (flexarray<-, sort-by) while similar things require import (complex functions, ratio functions). Hard to know without trying.

## Things That Work Nicely

### Method-as-value with ~.method
`(each ~.count strings)` is elegant - the term `{.count}` becomes callable.

### Pattern matching on terms
```cant
(may p
  (be {point x y} ...)
  (be {origin} ...)
  (else ...))
```
Clean and readable. Works great for building ADTs.

### Callable terms for message-passing
A term like `{.greet "world"}` can be called on an object: `(msg greeter)` is same as `(call greeter msg)`. Useful for deferred/stored messages.

### Range iteration with .till and .thru
`(3 .till 7)` gives you 3,4,5,6. `(3 .thru 7)` gives 3,4,5,6,7. Nice and readable.

### Terms as lightweight data structures
Using `{tag ...}` terms for ADTs is very clean - no boilerplate class definitions needed. Pattern matching makes them easy to use.

### Traits for delegation
The trait system is simple but effective. `{extend trait}` in make is clean syntax.

### The `for` sugar
`(for each [(x xs)] body)` reads nicely compared to `(each (on (x) body) xs)`.

### Module system
`export`/`import` are straightforward. Modules are just maps of names to values.

## Bugs Found

### ejector-protect + ejector = crash
`ejector-protect` is marked "TODO untested" in `player/player.scm:218`. When an ejector fires through an ejector-protect block, there's a match failure. The cleanup never runs. Works fine for normal completion.

```cant
;; This crashes:
(with-ejector (on (ej)
  (ejector-protect
    ($ (ej .eject 'early))
    ($ (out .say "cleanup\n")))))
```

### Ranges don't display their contents
`(0 .till 5)` displays as `#<range:_:march<->` instead of showing the actual values. Makes debugging harder - you have to `(as-list (0 .till 5))` to see contents.

## Design Observations

### Uniformity vs. Familiarity tradeoff
Making math operations methods is consistent with "everything is message passing" but fights decades of `sqrt(x)` muscle memory. The guide acknowledges this bikeshedding.

### Method discovery is hard
No built-in way to ask "what can I do with this object?" - you have to read source files or the guide. A `,methods obj` REPL command would help.

### Terms vs Objects decision
When to use `{tag ...}` terms vs `make` objects? Terms are values (structural equality, immutable, pattern-matchable). Objects have identity. The guide could give clearer guidance on this choice.

### Namespace pollution
The global environment has many names. This helps discoverability (things "just work") but causes shadowing errors and makes it hard to know what's global vs needs import.

## What Would Help New Users

1. **Method reference** - "What methods does X have?" for numbers, strings, lists, etc.
2. **Quick reference card** - Common operations in one page
3. **Error message improvements** - Show source locations, not AST reconstitutions
4. **REPL introspection** - `,methods obj`, `,apropos name`, `,doc name`
5. **Clearer global vs library distinction** - Document what's pre-loaded
6. **More examples in guide** - Show `.parts` for terms, `.sqrt` for numbers, etc.

## Ideas

- Fix ejector-protect bug
- Make ranges display their contents (at least for small finite ranges)
- Add REPL `,methods obj` command
- Document which names are taken globally
- Consistent naming: either `!set<-` or `set<-` for mutable, pick one
- Add `complex<-` and similar constructors for consistency (or document the term-based approach)
