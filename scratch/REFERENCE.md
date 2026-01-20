# Cant Reference

Systematic documentation of types, methods, and global functions.

## Numbers

All math is via methods, not global functions.

### Arithmetic
| Method | Description |
|--------|-------------|
| `(n .+ m)` | Addition |
| `(n .- m)` | Subtraction |
| `(n .* m)` | Multiplication |
| `(n .** m)` | Exponentiation (n^m) |
| `(n .quotient d)` | Integer division |
| `(n .remainder d)` | Remainder (sign of n) |
| `(n .modulo d)` | Modulo (sign of d) |
| `(n ./mod d)` | Returns tuple `{~ quotient remainder}` |
| `(n .*/mod m d)` | `((n * m) ./mod d)` |

### Numeric Functions
| Method | Description |
|--------|-------------|
| `n.sqrt` | Square root |
| `n.magnitude` | Absolute value (NOT `.abs`) |
| `n.sign` | Returns -1, 0, or 1 |
| `n.floor` | Floor |
| `n.ceiling` | Ceiling |
| `n.round` | Round to nearest integer |
| `(n .round digits)` | Round to n decimal places |
| `n.exp` | e^n |
| `n.ln` | Natural log |
| `(n .log base)` | Log base b |
| `n.exact` | Convert inexact to exact |
| `n.inexact` | Convert exact to inexact |

### Predicates
| Method | Description |
|--------|-------------|
| `n.even?` | Is n even? |
| `n.odd?` | Is n odd? |
| `(n .even? b)` | Is n divisible by b? |
| `n.positive?` | Is n > 0? |
| `n.negative?` | Is n < 0? |
| `(n .divides? b)` | Does n divide b evenly? |

### Bit Operations (integers)
| Method | Description |
|--------|-------------|
| `n.not` | Bitwise NOT |
| `(n .and m)` | Bitwise AND |
| `(n .or m)` | Bitwise OR |
| `(n .xor m)` | Bitwise XOR |
| `(n .<< k)` | Left shift |
| `(n .>> k)` | Right shift |
| `(n .bit i)` | Get bit i (0 or 1) |

### Ranges (returns lazy sequence)
| Method | Description |
|--------|-------------|
| `(a .till b)` | a, a+1, ..., b-1 |
| `(a .till b step)` | a, a+step, ... (< b) |
| `(a .thru b)` | a, a+1, ..., b |
| `(a .span n)` | a, a+1, ..., a+n-1 |
| `(a .down-thru b)` | a, a-1, ..., b |
| `(a .down-till b)` | a, a-1, ..., b+1 |
| `n.till` | 0, 1, ..., n-1 |
| `n.and-up` | n, n+1, n+2, ... (infinite) |

### Convenience
| Method | Description |
|--------|-------------|
| `n.+1` | n + 1 |
| `n.-1` | n - 1 |
| `n.!` | Factorial |
| `(n .choose k)` | Binomial coefficient |
| `n.text` | Convert to string |
| `(n .digits)` | List of digits (base 10) |
| `(n .digits base)` | List of digits in base |

---

## Texts (Strings)

Texts are immutable sequences of runes (characters).

### Access
| Method | Description |
|--------|-------------|
| `s.count` | Length |
| `(s i)` | Character at index i |
| `s.first` | First character |
| `s.rest` | All but first (returns list!) |
| `s.last` | Last character |
| `(s .from i)` | Substring from i to end |
| `(s .from i j)` | Substring from i to j (exclusive) |
| `(s .get i)` | Character at i, or #no |
| `(s .get i default)` | Character at i, or default |
| `(s .maps? i)` | Is i a valid index? |

### Search
| Method | Description |
|--------|-------------|
| `(s .find? val)` | Is val in s? |
| `(s .find val)` | Index of val (error if missing) |
| `(s .find val default)` | Index of val, or default |

### Predicates
| Method | Description |
|--------|-------------|
| `s.none?` | Is empty? |
| `s.some?` | Is non-empty? |

### Transform
| Method | Description |
|--------|-------------|
| `(chain s1 s2)` | Concatenate |
| `s.uppercase` | To uppercase |
| `s.lowercase` | To lowercase |
| `s.capitalize` | Capitalize first char |
| `s.trim` | Strip whitespace both ends |
| `(s .trim char)` | Strip specific char |
| `s.trim-left` | Strip leading whitespace |
| `s.trim-right` | Strip trailing whitespace |
| `(s .replace old new)` | Replace all occurrences |
| `(s .repeat n)` | Repeat n times |
| `(s .pad-left n)` | Pad with spaces to length n |
| `(s .pad-left n char)` | Pad with char |
| `(s .pad-right n)` | Pad right |
| `(s .center n)` | Center in width n |

### Split/Join
| Method | Description |
|--------|-------------|
| `s.split` | Split on whitespace |
| `(s .split delim)` | Split on delimiter string |
| `s.split-lines` | Split on newlines |
| `(sep .join list)` | Join strings with separator |

### Format
| Method | Description |
|--------|-------------|
| `(s .format @args)` | Format string (like printf) |

Format codes: `~d` display, `~w` write, `~~` literal tilde

### Conversion
| Method | Description |
|--------|-------------|
| `s.values` | List of characters |
| `s.symbol` | Convert to symbol |
| `s.number` | Parse as number (error if invalid) |
| `s.?number` | Parse as number or #no |

---

## Runes (Characters)

Written as `#\a`, `#\newline`, `#\space`, `#\tab`.

### Predicates
| Method | Description |
|--------|-------------|
| `r.letter?` | Is alphabetic? |
| `r.digit?` | Is 0-9? |
| `r.alphanumeric?` | Letter or digit? |
| `r.whitespace?` | Is whitespace? |
| `r.lowercase?` | Is lowercase? |
| `r.uppercase?` | Is uppercase? |
| `r.printable?` | Is printable ASCII? |

### Transform
| Method | Description |
|--------|-------------|
| `r.lowercase` | To lowercase |
| `r.uppercase` | To uppercase |
| `r.code` | Unicode code point |
| `r.text` | Convert to 1-char string |
| `(r .+ n)` | Char n positions later |
| `(r .- n)` | Char n positions earlier |
| `(r .- r2)` | Distance between chars |

---

## Lists

Immutable linked lists. Written as `'(1 2 3)` or `(list<- 1 2 3)`.

### Construction
| Expression | Description |
|------------|-------------|
| `'()` | Empty list |
| `(link x xs)` | Cons x onto xs |
| `(link x y zs)` | Cons multiple |
| `(list<- a b c)` | Make list |
| `(chain xs ys)` | Append |

### Access
| Method | Description |
|--------|-------------|
| `xs.count` | Length |
| `(xs i)` | Element at index |
| `xs.first` | First element |
| `xs.rest` | All but first |
| `xs.last` | Last element |
| `xs.but-last` | All but last |
| `(xs .from i)` | Drop first i |
| `(xs .from i j)` | Slice [i, j) |
| `(xs .prefix n)` | First n elements |
| `(xs .suffix n)` | Last n elements |
| `(xs .get i default)` | Element or default |

### Search
| Method | Description |
|--------|-------------|
| `(xs .maps? i)` | Valid index? |
| `(xs .find? val)` | Contains val? |
| `(xs .find val)` | Index of val |
| `(xs .find val default)` | Index or default |
| `(xs .prefix? ys)` | Does xs start with ys? |
| `(xs .suffix? ys)` | Does xs end with ys? |

### Predicates
| Method | Description |
|--------|-------------|
| `xs.none?` | Empty? |
| `xs.some?` | Non-empty? |

### Transform
| Method | Description |
|--------|-------------|
| `(xs .chain ys)` | Append |
| `(xs .repeat n)` | Repeat n times |
| `(xs .remove x)` | Remove all x |
| `(xs .pad-left n val)` | Pad to length n |
| `(xs .pad-right n val)` | Pad right |
| `(xs .join segments)` | Join with xs as separator |

### Combinatorics
| Method | Description |
|--------|-------------|
| `xs.permutations` | All permutations |
| `(xs .k-permutations k)` | k-permutations |
| `(xs .k-sets k)` | k-combinations |
| `(xs .k-bags k)` | k-combinations with repetition |
| `(xs .k-lists k)` | k-tuples (cartesian product) |
| `(xs .k-slices k)` | All length-k slices |
| `xs.prefixes` | All prefixes |
| `xs.suffixes` | All suffixes |
| `xs.slices` | All slices |
| `xs.cycle` | Infinite cycle |

### Conversion
| Method | Description |
|--------|-------------|
| `xs.text` | To string (if chars) |
| `xs.array` | To array |
| `xs.tuple` | To term tuple |
| `xs.keys` | Indices as range |
| `xs.values` | Self |
| `xs.items` | Enumerated `{~ i x}` pairs |

---

## Arrays

Mutable, fixed-size. Written as `[a b c]` or `(array<- a b c)`.

### Construction
| Expression | Description |
|------------|-------------|
| `(array<- a b c)` | Make array |
| `(array<-count n val)` | n copies of val |
| `(array<-list xs)` | From list |
| `arr.copy` | Shallow copy |

### Access/Modify
| Method | Description |
|--------|-------------|
| `arr.count` | Length |
| `(arr i)` | Element at i |
| `(arr .set! i val)` | Set element |
| `(arr .from i)` | Subarray from i |
| `(arr .from i j)` | Subarray [i, j) |
| `(arr .chain arr2)` | Concatenate (new array) |
| `(arr .move! dst src lo hi)` | Block copy |

Note: Arrays deliberately lack `.first`/`.rest` to discourage quadratic traversal. Use `.values` to get a traversable list.

### Conversion
| Method | Description |
|--------|-------------|
| `arr.values` | To list |
| `arr.list` | To list |
| `arr.text` | To string (if chars) |

---

## Maps

### Immutable Maps
```cant
(map<- {~ 'a 1} {~ 'b 2})
(map<-items items-list)
(map<-lists '((a 1) (b 2)))
```

### Mutable Maps
```cant
(!map<-)              ; empty
(!map<-items items)
(!map<-lists alists)
```

### Access
| Method | Description |
|--------|-------------|
| `(m key)` | Value for key (error if missing) |
| `(m .get key)` | Value or #no |
| `(m .get key default)` | Value or default |
| `(m .maps? key)` | Has key? |
| `m.count` | Number of entries |
| `m.keys` | List of keys |
| `m.values` | List of values |
| `m.items` | List of `{~ key value}` tuples |

### Search
| Method | Description |
|--------|-------------|
| `(m .find? val)` | Has value? |
| `(m .find val)` | Key for value |
| `(m .find val default)` | Key or default |
| `m.domain` | Keys as set |
| `m.range` | Values as set |
| `m.inverse` | Swap keys/values |

### Predicates
| Method | Description |
|--------|-------------|
| `m.none?` | Empty? |
| `m.some?` | Non-empty? |
| `(m .intersects? m2)` | Share any keys? |
| `(m .disjoint? m2)` | No shared keys? |
| `(m .subset-of? m2)` | All my keys in m2? |
| `m.distinct?` | All values unique? |

### Mutable Operations
| Method | Description |
|--------|-------------|
| `(m .set! key val)` | Set entry |
| `(m .delete! key)` | Remove entry |
| `(m .get-set! key thunk)` | Get or compute and set |
| `m.clear!` | Remove all |

### Immutable Operations
| Method | Description |
|--------|-------------|
| `(m .set key val)` | New map with entry |
| `(m .override m2)` | Merge, m2 wins |
| `m.copy` | Mutable copy |

---

## Sets

Mutable sets. (Note: `set<-`, not `!set<-` despite being mutable.)

```cant
(set<- 1 2 3)
(set<-list xs)
```

Sets are maps where values are membership counts. Use map methods plus:

| Method | Description |
|--------|-------------|
| `(s .add! x)` | Add element |
| `(s .maps? x)` | Contains x? |
| `(s .merge! other)` | Add all from other |
| `(s .intersect other)` | Intersection (new set) |

---

## Terms

Immutable tagged data: `{tag arg1 arg2 ...}`. Tuples use `~` as tag: `{~ a b}`.

| Method | Description |
|--------|-------------|
| `t.tag` | The tag symbol |
| `t.parts` | Arguments as list |
| `(t .part n)` | Nth argument |

Terms as callable: `({.method arg} object)` = `(object .method arg)`

Construction: `(term<- tag args-list)` or just `{tag ...}` syntax.

---

## Boxes

Mutable cells for single values.

```cant
(let b (box<- initial-value))
```

| Method | Description |
|--------|-------------|
| `b.^` | Read value |
| `(b .^= val)` | Write value |
| `(b .update f)` | Apply f and store result |
| `b.+1!` | Increment (returns new value) |
| `b.-1!` | Decrement |
| `(b .+= n)` | Add n |
| `(b .-= n)` | Subtract n |

---

## Global Functions

### Iteration
| Function | Description |
|----------|-------------|
| `(each f xs)` | Map f over xs |
| `(each f xs ys)` | Map f over pairs |
| `(each! f xs)` | Map for side effects |
| `(keep pred xs)` | Filter |
| `(skip pred xs)` | Filter out |
| `(fold f xs init)` | Right fold: `(f x1 (f x2 (f x3 init)))` |
| `(fold1 f xs)` | Right fold without init |
| `(amass f init xs)` | Left fold: `(f (f (f init x1) x2) x3)` |
| `(amass1 f xs)` | Left fold without init |
| `(some pred xs)` | Any match? (returns first truthy) |
| `(every pred xs)` | All match? |
| `(yeahs maybe xs)` | Filter out #no results |

### Aggregation
| Function | Description |
|----------|-------------|
| `(sum xs)` | Sum of numbers |
| `(sum-by f xs)` | Sum of (f x) |
| `(tally xs)` | Sum of counts |
| `(tally-by f xs)` | Sum of (.count (f x)) |
| `(min @xs)` | Minimum |
| `(max @xs)` | Maximum |
| `(min-by f xs)` | Element with min (f x) |
| `(max-by f xs)` | Element with max (f x) |

### Sorting
| Function | Description |
|----------|-------------|
| `(sort xs)` | Sort by natural order |
| `(sort-by f xs)` | Sort by key function |

### List Operations
| Function | Description |
|----------|-------------|
| `(link x xs)` | Cons |
| `(chain xs ys ...)` | Append |
| `(reverse xs)` | Reverse |
| `(zip xs ys)` | Pair up elements |
| `(zip @lists)` | Tuple up elements |
| `(zip .ragged @lists)` | Zip, clip to shortest |
| `(transpose lists)` | Transpose matrix |
| `(grid* xs ys)` | Cartesian product |
| `(enumerate xs)` | List of `{~ i x}` |

### Control
| Function | Description |
|----------|-------------|
| `(-> x f g h)` | Pipeline: `(h (g (f x)))` |
| `(compose f g)` | Function composition |
| `(itself x)` | Identity function |
| `(mayhap f x)` | `(f x)` unless x is #no |

### Maps
| Function | Description |
|----------|-------------|
| `(map-by f keys)` | Map from k to (f k) |
| `(map<-values f vals)` | Map from (f v) to v |
| `(each-value f map)` | Transform all values |
| `(where pred map)` | Keys where value matches |

### Predicates
| Function | Description |
|----------|-------------|
| `(= a b)` | Equal? |
| `(not= a b)` | Not equal? |
| `(< a b)`, `(> a b)` | Comparison |
| `(<= a b)`, `(>= a b)` | Comparison |
| `(<=> a b)` | Both <= and >= (equivalent) |
| `(not x)` | Boolean not |
| `(link? x)` | Is cons cell? |
| `(list? x)` | Is list? |
| `(number? x)` | Is number? |
| `(integer? x)` | Is integer? |
| `(count? x)` | Is non-negative integer? |
| `(text? x)` | Is string? |
| `(symbol? x)` | Is symbol? |
| `(rune? x)` | Is character? |
| `(array? x)` | Is array? |
| `(term? x)` | Is term? |
| `(box? x)` | Is box? |

### I/O
| Function | Description |
|----------|-------------|
| `(out .say fmt @args)` | Print formatted |
| `(out .display x)` | Print without quotes |
| `(out .write x)` | Print with quotes |
| `in.read-rune` | Read one character |
| `in.read-line` | Read one line |
| `(read source)` | Read one S-expression |
| `(read-all source)` | Read all S-expressions |
| `(open-input-file path)` | Open for reading |
| `(open-output-file path)` | Open for writing |
| `(with-input-file f path)` | Open, call f, close |
| `(with-output-file f path)` | Open, call f, close |

### Error Handling
| Function | Description |
|----------|-------------|
| `(oops msg @args)` | Raise error |
| `(surely test @msg)` | Assert |
| `(with-ejector f)` | Create ejector, call f |
| `(ej .eject val)` | Non-local return |
| `(ejector-protect thunk cleanup)` | Unwind protection (BUGGY) |

---

## Syntax Quick Reference

### Definitions
```cant
(let x 42)                    ; bind variable
(to (f x) body)               ; define function
(to ((f x) y) body)           ; curried function
(make name (to ~.m body) ...) ; define object
```

### Expressions
```cant
(if test then else)
(when test body...)           ; if without else
(unless test body...)
(and a b c)
(or a b c)
(do [bindings] body...)       ; let block
(do name [bindings] body...)  ; named let (loop)
(so expr1 expr2 ...)          ; sequence
(on (args) body)              ; lambda
($ body)                      ; thunk (zero-arg lambda)
||body                        ; lambda with implicit `it`
```

### Pattern Matching
```cant
(may val
  (be pattern body)
  (be pattern body)
  (else body))

(given
  (be pattern body)
  (be pattern body))          ; anonymous pattern function
```

### Patterns
```cant
_                             ; wildcard
x                             ; bind to x
42                            ; literal
'foo                          ; quoted literal
{tag p1 p2}                   ; term with subpatterns
(list<- p1 p2)                ; list
[p1 p2]                       ; array
`(,p1 ,p2)                    ; quasiquote
(? pred)                      ; predicate guard
(? pred pattern)              ; guarded pattern
(= expr)                      ; equality check
(-- p1 p2)                    ; and-pattern
(-> f pattern)                ; view pattern
{tag @rest}                   ; rest pattern
```

### `hm` (Chained Conditionals)
```cant
(hm (if test1 result1)
    (if test2 result2)
    (let x expr)              ; bind and continue
    (when test result)        ; like if
    (unless test result)
    (else default))
```

### `for` (Iterator Sugar)
```cant
(for each [(x xs)] body)      ; = (each (on (x) body) xs)
(for keep [(x xs)] body)
(for amass [(acc init) (x xs)] body)
(for fold [(x xs) (acc init)] body)
```
