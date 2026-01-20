# Testing & Documentation Triage

Notes on test coverage, documentation, and completeness for library/ and examples/.

## Test Infrastructure Assessment

### Current State
- **Output diff testing only**: `./t foo` runs `test/test-foo.cant`, captures output, diffs against `foo.expected`
- **No assertion framework**: Tests print output; no structured pass/fail
- **`surely` is the only assertion primitive** - triggers an oops on failure
- **`squickcheck`** provides property-based testing but is underused

### Ideas for Improvement

1. **Add `assert=` / `check` utilities** that print pass/fail:
   ```cant
   (to (check label expected actual)
     (if (= expected actual)
         (out .say "PASS: ~d\n" label)
         (out .say "FAIL: ~d - expected ~w, got ~w\n" label expected actual)))
   ```
   Advantage: Still works with diff-based testing but shows what failed.

2. **A test harness that counts pass/fail**:
   ```cant
   (let tester (test-runner<-))
   (tester .check "addition" (+ 2 2) 4)
   (tester .report)  ;; "17 passed, 2 failed"
   ```

3. **Use squickcheck more widely** - it already exists but only `squickcheck-examples.cant` and a few tests use it.

4. **Add `./t all`** to run all test-*.cant files (not just the 3 in run-tests).

5. **Better failure reporting**: Currently if a test changes output, diff shows the difference but doesn't indicate *which test case* within the file failed.

6. **REPL `,test` command** for quick ad-hoc testing during development.

---

## Library Triage

### Complete / Well-tested
| Module | Description | Has Test | Notes |
|--------|-------------|----------|-------|
| `bag.cant` | Multisets | test-bag | Some TODOs for more ops |
| `chaos.cant` | PRNG | test-random | |
| `complex.cant` | Complex numbers | test-complex | No constructor (use term directly) |
| `english-numbers.cant` | Number to words | test-english-numbers | |
| `factoring.cant` | Prime factorization | test-factor | Has internal test fn |
| `flexarray.cant` | Growable arrays | test-flexarray | Already global |
| `format-tables.cant` | Table formatting | test-format-tables | |
| `memoize.cant` | Memoization | test-memoize | Tiny but complete |
| `pairing-heap.cant` | Priority queues | test-pairing-heap | Well-documented |
| `queue.cant` | Functional FIFOs | (no separate test) | Small, looks correct |
| `ratio.cant` | Rationals | test-ratio | |
| `regex-match.cant` | Regex matching | test-regex-match | |
| `regex-gen.cant` | Regex generation | test-regex-gen | |
| `roman.cant` | Roman numerals | test-roman | |
| `sort.cant` | Generic sorting | test-sort | |
| `sset.cant` | Sorted sets | test-sset | |
| `text-find.cant` | Boyer-Moore search | test-text-find | |
| `text-wrap.cant` | Text wrapping | test-text-wrap | |
| `unify.cant` | Unification | test-unify | "XXX needs a lot of work" |
| `cycle-write.cant` | Cyclic structure printing | test-cycle-write | |

### Partially Complete / Needs Work
| Module | Description | Has Test | Issues |
|--------|-------------|----------|--------|
| `ansi-terminal.cant` | ANSI escape codes | no | "TODO factor wrt sturm" |
| `bdd.cant` | Binary decision diagrams | no | "TODO export more, rename" |
| `bounded-writer.cant` | Truncating writer | no | "TODO warning-track state" |
| `calendar.cant` | Date/time | no | "TODO tests", many TODOs, limited year range |
| `csp.cant` | Constraint solving | no | Works but sparse docs |
| `dd.cant` | Decision diagrams | test-dd | |
| `debugger.cant` | Interactive debugger | no | Complex, needs powers |
| `flextext.cant` | Gap buffer | test-flextext | "XXX untested" on some methods |
| `grid-2d.cant` | 2D arrays | no | "TODO add to tests" |
| `parson.cant` | PEG parsing | test-parson | Main parser, many features |
| `parson-core.cant` | PEG core | test-parson-squared | |
| `polynomials.cant` | Polynomials | no | "TODO tests" |
| `pretty-layout.cant` | Wadler pretty printing | test-pretty-print | |
| `pretty-print.cant` | S-expr pretty printing | test-pretty-print | |
| `squickcheck.cant` | Property testing | squickcheck-examples | Framework itself |
| `sturm.cant` | Terminal UI | no | Complex, interactive |
| `traceback.cant` | Error traces | (via failing.cant) | Works |

### Incomplete / Stub
| Module | Description | Issue |
|--------|-------------|-------|
| `minikanren.cant` | Logic programming | "XXX unfinished untested" |
| `mostly-linear.cant` | Linear equation solver | "XXX unfinished", has stubs |

### Missing Tests Priority
1. **calendar.cant** - Explicitly asks for tests
2. **grid-2d.cant** - Explicitly asks for tests
3. **polynomials.cant** - Explicitly asks for tests
4. **csp.cant** - Complex module, needs validation
5. **bdd.cant** - Complex, could use examples

---

## Examples Triage

### Categories

**Automata** (7 files) - All seem complete:
- `game-of-life.cant` - Complete, exports test data
- `turing-machine.cant` - Complete with smoke-test
- `elementary-ca.cant`, `fractran.cant`, `popcode.cant`, `slow-life.cant`, `text-register-machine.cant` - All in quick test

**Benchmarks** (2 files):
- `dumb-prime-benchmark.cant`, `sieve.cant` - For performance comparison

**Brainfuck** (1 file):
- Complete interpreter + compiler, tested

**Coding Theory** (4 files):
- `hamming-codes.cant`, `huffman-coding.cant`, `lzw-compression.cant`, `weighted-code.cant`
- All tested in quick test

**Dole Editor** (9 files):
- Full text editor implementation
- Complex, interactive - hard to auto-test

**Exercises/Puzzles** (9 files):
- Good variety, most tested
- `ghost.cant` - word game, interactive
- `cryptarithmetic-*.cant` - solver examples

**Games** (12 files):
- `tictactoe.cant`, `sokoban.cant`, `2048.cant` - Complete, have test hooks
- `chess.cant`, `connect4.cant` - Game logic
- `wordles.cant` - Word game
- Interactive games hard to auto-test

**Lambda Calculus** (7 files):
- `lambdaterp.cant`, `lambdacompiler.cant`, `compact-lambda.cant` - All tested
- Substantial interpreters

**Project Oiler** (43 files):
- Project Euler solutions
- Has own test harness (test-oiler, test-oiler-further)

**Rosetta Code** (3 files):
- Simple examples from rosettacode.org

**Text Processing** (6 files):
- `anagram-words.cant`, `kwic.cant`, `markov-text.cant`, `most-common.cant`, `oodles.cant`, `spelling.cant`
- Utilities for text analysis

**Other Notable**:
- `cant.cant` - Metacircular interpreter
- `csv.cant` - CSV parsing
- `maze.cant` - Maze generation
- `squickcheck-examples.cant` - Shows how to use property testing
- `failing.cant` - Tests error handling

### Examples Not Tested
The following are NOT in test-quick or test-omnibus:
- Most games (interactive)
- dole-editor (interactive)
- Most terminal/* (interactive)
- benchmarks/* (intentional)
- capability-patterns/revoke.cant
- code-golf/* (probably should add)
- Some rosetta-code/*

### Suggestions
1. Add smoke tests for more games (like tictactoe.quick-test)
2. The interactive examples could have non-interactive test modes
3. j-bob-proof-checker/ has its own smoketest - add to run-tests?

---

## Documentation Gaps

### In Library
- **parson.cant** - Complex parser combinator library, needs tutorial/examples
- **sturm.cant** - Terminal UI, needs usage guide
- **csp.cant** - Constraint solving, needs problem examples
- **bdd.cant** / **dd.cant** - Decision diagrams need explanation

### General
- No API reference beyond the source
- REFERENCE.md (in scratch/) helps but isn't official
- Many modules have good header comments but some are sparse

---

## Action Items

### Quick Wins (add tests for modules that ask for them)
1. Create test-calendar.cant
2. Create test-grid-2d.cant
3. Create test-polynomials.cant
4. Add smoke tests to more games

### Medium Effort
5. Implement simple test-runner utility
6. Make squickcheck usage more widespread
7. Document parson with examples

### Larger Projects
8. Design and implement a proper test framework
9. Add interactive test modes to games
10. Create API documentation generator
