# [Reeder: a toy Lisp reader](https://github.com/tfeb/reeder "reeder")
This repo contains a toy table-driven reader for Lisp, written in Common Lisp.  It is not an attempt to reproduce the CL reader and still less is it intended for serious use: rather it's meant to be something to play with which is small enough to understand.  It is extensible: you can define macro characters (all macro characters are terminating in CL terms), define your own parsers for tokens, control what is treated as whitespace, define your own token reader and so on.  There are configurable single and multiple escapes which by default are `\` and `|` respectively.  Everything lives in objects called 'reedtables'[^1]: you can copy these to make your own, including making a copy of the hidden 'precursor reedtable' with the standard behaviour.

The default configuration (copied from the precursor reedtable) has exactly two read macros:

- `"` reads strings;
- `(` reads lists and conses (there is in fact another macro character, `)` which just raises an error).

So in particular there is no readmacro for `#` or even `'` in the default reedtable: there is an example which shows how to add a quote reader, which is easy to do.

There are provided token parsers which can parse integers and rationals: anything else is parsed as a symbol.  Token parsing is easily extended, as is the notion of what is a token.

The implementation depends on [CL-PPCRE](https://edicl.github.io/cl-ppcre/ "CL-PPCRE"), using its sexp notation for regexps to drive token parsers, and [org.tfeb.hax.iterate](https://tfeb.github.io/tfeb-lisp-hax/ "org.tfeb.hax") for applicative iteration.  You may need to fiddle with the dependencies to get it to build.  It's only been used in the extremely wonderful [LispWorks](http://www.lispworks.com "LispWorks"): it should be portable CL though.

Other than this sketch there is currently no real documentation[^2]: you will need to read the source to know how to use it.  On the other hand it is less than 600 lines long.

----

Caveat emptor: monsters still lurk in the shadows.

[^1]:	I make no apologies for the pun.

[^2]:	There should be documentation on argument and return conventions, at least: one day.