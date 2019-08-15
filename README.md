# Cant, a message-oriented language

This is a hobby project: a programming language descended mainly from
[E](http://erights.org/) and Scheme. Nowadays there are at least two
more direct and more interesting and useful E descendants: [Secure
EcmaScript](https://github.com/Agoric/SES) and
[Monte](https://monte.readthedocs.io/en/latest/). You probably want to
look at those instead.

## So, the message is "buzz off"?

Well, no, but I'm changing my mind all the time; even given a usable
implementation (it's totally not), your own code could rot in
days. You might at most perhaps enjoy playing around.

## What's so great about this, at least in aspiration?

Re-working the Actors roots of Scheme *could* be neat. 

In the short term, I wanted a Scheme that didn't often make me miss
Python. (I was a Schemer before, and it never sat with me right that
Python had not only more libraries but often greater concision.) This
much is kind of done, I'd say, except for imperative-style code.

In the longer term I'd like to support a Smalltalky programming
environment. This goal motivated the incredibly slow initial
implementation strategy, where the interpreter internals always appear
as meta-objects with no optimization, and even primitive objects
execute via high-level scripts.

Another feature which I'm not claiming is great is the bikeshedding of
the names of everything without regard to familiarity. I think the
language name gives fair enough warning.

## Example code?

```
$ ./cant       # You need Chez Scheme to run this.
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
$ ./cant eg/fizzbuzz.scm
```

Or from the listener:

```
$ ./cant
-> (load "eg/fizzbuzz.scm")
1
2
Fizz
...

```

## Documentation?

Not yet, but there's an abortive start at
[overview.md](https://github.com/darius/cant/blob/master/overview.md).

## Isn't 'Cant' the [Chevy Nova](https://www.snopes.com/fact-check/chevrolet-nova-name-spanish/) of language names?

Expressing *inabilities* in code is still underemphasized; I'm just
fine with a name that calls attention to what you can't do. (Although
it's only aspirational for now.)

## Thanks

Of course, this project would not exist without Scheme or E. It's
pretty likely to get more like E, because so far I've been consulting
my decades-old memory instead of the sources, and E probably did
things better.

Thanks to Kragen Sitaker for early discussions about the language.

Some early work was done at [Recurse Center](https://www.recurse.com),
a great place for feedback.
