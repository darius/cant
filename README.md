# Squeam

Yes, another Scheme dialect;  
or, lambda: the germinal object.  
So object-oriented, it'll turn your stomach!  

Running, for the moment, on Gambit-Scheme.
You can try it out with:

$ gsi testme.scm


## The motivation, roughly

I gave up waiting for so much of what I enjoyed in
[E](http://erights.org/) to appear again together in a popular
language. E is a big language, and here, to start, I'm building up
only some of the simplest aspects, unfaithfully; but along the way I
can also cater to some of my own idiosyncrasies. I want to make it a
whole system in the Smalltalk style, with meta-access for debugging,
etc. That and just getting together a language I can program in are
overriding any actual security so far: despite the inspiration, there
is no security, and no distribution, not even an event loop.

It ought to remain a simpler language than E even when developed: an
object is a thing that pattern-matches on the messages it
receives. There are fewer kinds of syntactic forms. It's a personal
artisanal hipster language.

Another goal: to be pithier than Scheme. Much of how Python beat
Scheme out for me in practice was the concise use of convenient
collection types. Along the way I renamed many Scheme forms to flow
better (idiosyncrasies, remember).

The goal of Smalltalkiness motivated the incredibly slow
implementation strategy to start with, where the interpreter internals
always appear as meta-objects with no optimization, and even primitive
objects execute via high-level scripts (in lib/runtime.scm).
