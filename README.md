# Cant

## A message-oriented language

This is a hobby project: a programming language descended mainly from
[E](http://erights.org/) and Scheme. Nowadays there are at least two
more direct and more interesting and useful E descendants: [Secure
EcmaScript](https://github.com/Agoric/SES) and
[Monte](https://monte.readthedocs.io/en/latest/). You probably want to
look at those instead.

## So, the message is "buzz off"?

Well, no, but I'm changing my mind all the time; even given a usable
implementation (it's totally not), your own code could rot in
days. You might perhaps enjoy playing around, at most.

## What's so great about this, at least in aspiration?

Re-working the Actors roots of Scheme *could* be neat. 

In the short term, I wanted a Scheme that didn't often make me miss
Python. I'd call that achieved now, except for imperative-style code.

In the longer term I'd like to support a sort of Smalltalky
programming environment. This goal motivated the incredibly slow
initial implementation strategy, where the interpreter internals
always appear as meta-objects with no optimization, and even primitive
objects execute via high-level scripts.

## Example code?

```
$ ./cant.scm       # You need Chez Scheme to run this.
-> (format "Hey, world.\n")
Hey, world.
-> (to (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
#<fib>
-> (fib 10)
89
-> 
```

## Well, *interesting* example code?

Dunno, but there are
[eg/](https://github.com/darius/cant/tree/master/eg) and
[library/](https://github.com/darius/cant/tree/master/library). You
can run a file from the shell like

```
$ ./cant.scm eg/fizzbuzz.scm
```

Or from the listener:

```
$ ./cant.scm
-> (load "eg/fizzbuzz.scm")
1
2
Fizz
...

```

## Documentation?

Not yet, but there's an abortive start in overview.md.

## Isn't 'Cant' the [Chevy Nova](https://www.snopes.com/fact-check/chevrolet-nova-name-spanish/) of language names?

Expressing *inabilities* in code is still underemphasized; I'm just
fine with a name calling attention to what you can't do. (It's only
aspirational for now, though.)

## Thanks

Of course, this project would not exist without Scheme or E. It's
pretty likely to get more like E, because so far I've been using my
decades-old memory instead of actually consulting it, and E probably
did things better.

Thanks to Kragen Sitaker for early discussions about the language.

Some early work was done at [Recurse Center](https://www.recurse.com),
a great place for feedback.
