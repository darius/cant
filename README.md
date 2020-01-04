# Cant, a message-oriented language

This is a hobby project: a programming language descended mainly from
[E](http://erights.org/) and Scheme. Nowadays there are at least two
more direct and more interesting and useful E descendants: [Secure
EcmaScript](https://github.com/Agoric/SES) and
[Monte](https://monte.readthedocs.io/en/latest/). Those are
incomparably more useful.

Why make this, then? I've long wanted a computing system that's
completely explained, simple enough to grasp in full detail without
getting bored or overwhelmed, and powerful enough that I'd actually
use it. Wirth's Oberon is one inspiring example of such a system. I'm
aiming at another according to my own taste and idiosyncrasies. Call
it a learning project, or outsider art.

### So the message is "buzz off"?

Just don't expect any stability, at least anytime soon.

## What's done so far?

A new Lisp dialect with a molasses-slow interpreter and an
almost-useless debugger. On the bright side, it's pretty close now to
offering capability security, and I'm sometimes pleased with the
syntax and the library. I think it's more concise than Scheme without
becoming more cryptic, once you get familiar.

### Example code?

```
$ ./incant       # You need Chez Scheme to run this.
-> (out .say "Hey, world.\n")
Hey, world.
-> (to (fib n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2)))))
#<fib>
-> (fib 10)
89
-> 
```

### Any *interesting* example code?

Dunno, but you can browse
[eg/](https://github.com/darius/cant/tree/master/eg) and
[library/](https://github.com/darius/cant/tree/master/library). See
[eg/games](https://github.com/darius/cant/tree/master/eg/games) and
[eg/automata](https://github.com/darius/cant/tree/master/eg/automata)
for maybe-fun starting points. Run a file from the shell like so:

```
$ ./incant eg/fizzbuzz.cant
```

Or from the listener:

```
$ ./incant
-> (load "eg/fizzbuzz.cant")
1
2
Fizz
...

```

### Documentation?

There's an abortive start at
[overview.md](https://github.com/darius/cant/blob/master/overview.md).

### Thanks

Of course, this project would not exist without Scheme or E. It's
pretty likely to get more like E, because so far I've been consulting
my decades-old memory instead of the sources, and E probably did
things better.

Thanks to Kragen Sitaker for early discussions about the language.
