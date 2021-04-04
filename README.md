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

Here is part of [this SO answer](https://stackoverflow.com/a/66942119/5920214 "SO answer") I wrote about reeder which may help.

[...]

So the critical features of any parser for a Lisp-family language are:

- it should not make any decision about the semantics of the language that it can avoid making;
- the structure it constructs must be available to the language itself as first-class objects;
- (and optionally) the parser should be modifiable from the language itself.

As examples:

- it is probably inevitable that the parser needs to make decisions about what is and is not a number and what sort of number it is;
- it would be nice if it had default handling for strings, but this should ideally be controllable by the user;
- it should make no decision at all about what, say `(< x y)` means but rather should return a structure representing it for interpretation by the language.

The reason for the last, optional, requirement is that Lisp-family languages are used by people who are interested in using them for implementing languages.  Allowing the reader to be altered from within the language makes that hugely easier, since you don't have to start from scratch each time you want to make a language which is a bit like the one you started with but not completely.

## Parsing Lisp
The usual approach to parsing Lisp-family languages is to have machinery which will turn a sequence of characters into a sequence of s-expressions consisting of objects which are defined by the language itself, notably symbols and conses (but also numbers, strings &c).  Once you have this structure you then walk over it to interpret it as a program: either evaluating it on the fly or compiling it.  Critically, you can also write programs which manipulate this structure itself: macros.

In 'traditional' Lisps such as CL this process is explicit: there is a 'reader' which turns a sequence of characters into a sequence of s-expressions, and macros explicitly manipulate the list structure of these s-expressions, after which the evaluator/compiler processes them.  So in a traditional Lisp `(< x y)` would be parsed as (a cons of a symbol `<` and (a cons of a symbol `x` and (a cons of a symbol `y` and the empty list object)), or `(< . (x . (y . ())))`, and this structure gets handed to the macro expander and hence to the evaluator or compiler.

In Scheme it is a little more subtle: macros are specified (portably, anyway) in terms of rules which turn a bit of syntax into another bit of syntax, and it's not (I think) explicit whether such objects are made of conses & symbols or not.  But the structure which is available to syntax rules needs to be as rich as something made of conses and symbols, because syntax rules get to poke around inside it.  If you want to write something like the following macro:

```lisp
(define-syntax with-silly-escape
  (syntax-rules ()
    [(_ (escape) form ...)
     (call/cc (λ (c)
                (define (escape) (c 'escaped))
                form ...))]
    [(_ (escape val ...) form ...)
     (call/cc (λ (c)
                (define (escape) (c val ...))
                form ...))]))
```

then you need to be able to look into the structure of what came from the reader, and that structure needs to be as rich as something made of lists and conses.

## A toy reader: [reeder](https://tfeb.github.io/reeder/ "reeder")
Reeder is a little Lisp reader written in Common Lisp that I wrote a little while ago for reasons I forget (but perhaps to help me learn [CL-PPCRE](https://edicl.github.io/cl-ppcre/ "CL-PPRCE"), which it uses).  It is emphatically a toy, but it is also small enough and simple enough to understand: certainly it is much smaller and simpler than the standard CL reader, and it demonstrates one approach to solving this problem.  It is driven by a table known as a reedtable which defines how parsing proceeds.

So, for instance:

```lisp
> (with-input-from-string (in "(defun foo (x) x)")
    (reed :from in))
(defun foo (x) x)
```

### Reeding
To read (reed) something using a reedtable:

1. look for the next interesting character, which is the next character not defined as whitespace in the table (reedtables have a configurable list of whitespace characters);
2. if that character is defined as a macro character in the table, call its function to read something;
3. otherwise call the table's token reader to read and interpret a token.

### Reeding tokens
The token reader lives in the reedtable and is responsible for accumulating and interpreting a token:

1. it accumulates a token in ways known to itself (but the default one does this by just trundling along the string handling single (`\`) and multiple (`|`) escapes defined in the reedtable until it gets to something that is whitespace in the table);
2. at this point it has a string and it asks the reedtable to turn this string into something, which it does by means of token parsers.

There is a small kludge in the second step: as the token reader accumulates a token it keeps track of whether it is 'denatured' which means that there were escaped characters in it.  It hands this information to the token parsers, which allows them, for instance, to interpret `|1|`, which is denatured, differently to `1`, which is not.

Token parsers are also defined in the reedtable: there is a `define-token-parser` form to define them.  They have priorities, so that the highest priority one gets to be tried first and they get to say whether they should be tried for denatured tokens.  Some token parser should always apply: it's an error if none do.

The default reedtable has token parsers which can parse integers and rational numbers, and a fallback one which parses a symbol.  Here is an example of how you would replace this fallback parser so that instead of returning symbols it returns objects called 'cymbals' which might be the representation of symbols in some embedded language:

Firstly we want a copy of the reedtable, and we need to remove the symbol parser from that copy (having previously checked its name using `reedtable-token-parser-names`).

```lisp
(defvar *cymbal-reedtable* (copy-reedtable nil))
(remove-token-parser 'symbol *cymbal-reedtable*)
```

Now here's an implementation of cymbals:

```lisp
(defvar *namespace* (make-hash-table :test #'equal))

(defstruct cymbal
  name)

(defgeneric ensure-cymbal (thing))

(defmethod ensure-cymbal ((thing string))
  (or (gethash thing *namespace*)
      (setf (gethash thing *namespace*)
            (make-cymbal :name thing))))

(defmethod ensure-cymbal ((thing cymbal))
  thing)
```

And finally here is the cymbal token parser:

```lisp
(define-token-parser (cymbal 0 :denatured t :reedtable *cymbal-reedtable*)
    ((:sequence
      :start-anchor
      (:register (:greedy-repetition 0 nil :everything))
      :end-anchor)
     name)
  (ensure-cymbal name))
```

An example of this.  Before modifying the reedtable:

```lisp
> (with-input-from-string (in "(x y . z)")
    (reed :from in :reedtable *cymbal-reedtable*))
(x y . z)
```

After:

```lisp
> (with-input-from-string (in "(x y . z)")
    (reed :from in :reedtable *cymbal-reedtable*))
(#S(cymbal :name "x") #S(cymbal :name "y") . #S(cymbal :name "z"))
```

### Macro characters
If something isn't the start of a token then it's a macro character.  Macro characters have associated functions and these functions get called to read one object, however they choose to do that.  The default reedtable has two-and-a-half macro characters:

- `"` reads a string, using the reedtable's single & multiple escape characters;
- `(` reads a list or a cons.
- `)` is defined to raise an exception, as it can only occur if there are unbalanced parens.

The string reader is pretty straightforward (it has a lot in common with the token reader although it's not the same code).

The list/cons reader is mildly fiddly: most of the fiddliness is dealing with consing dots which it does by a slightly disgusting trick: it installs a secret token parser which will parse a consing dot as a special object if a dynamic variable is true, but otherwise will raise an exception.  The cons reader then binds this variable appropriately to make sure that consing dots are parsed only where they are allowed.  Obviously the list/cons reader invokes the whole reader recursively in many places.

And that's all the macro characters.  So, for instance in the default setup, `'` would read as a symbol (or a cymbal).  But you can just install a macro character:

```lisp
(defvar *qr-reedtable* (copy-reedtable nil))

(setf (reedtable-macro-character #\' *qr-reedtable*)
      (lambda (from quote table)
        (declare (ignore quote))
        (values `(quote ,(reed :from from :reedtable table))
                (inch from nil))))
```

And now `'x` will read as `(quote x)` in `*qr-reedtable*`.

Similarly you could add a more compllicated macro character on `#` to read objects depending on their next character in the way CL does.

An example of the quote reader.  Before:

```lisp
> (with-input-from-string (in "'(x y . z)")
    (reed :from in :reedtable *qr-reedtable*))
\'
```

The object it has returned is a symbol whose name is `"'"`, and it didn't read beyond that of course.  After:

```lisp
> (with-input-from-string (in "'(x y . z)")
    (reed :from in :reedtable *qr-reedtable*))
`(x y . z)
```

### Other notes
Everything works one-character-ahead, so all of the various functions get the stream being read, the first character they should be interested in and the reedtable, and return both their value and the next character.   This avoids endlessly unreading characters (and probably tells you what grammar class it can handle natively (obviously macro character parsers can do whatever they like so long as things are sane when they return).

It probably doesn't use anything which isn't moderately implementable in non-Lisp languages.  Some

- Macros will cause pain in the usual way, but the only one is `define-token-parser`.  I think the solution to that is the usual expand-the-macro-by-hand-and-write-that-code, but you could probably help a bit by having an `install-or-replace-token-parser` function which dealt with the bookkeeping of keeping the list sorted etc.
- You'll need a language with dynamic variables to implement something like the cons reeder.
- it uses CL-PPCRE's s-expression representation of regexps.  I'm sure other languages have something like this (Perl does) because no-one wants to write stringy regexps: they must have died out decades ago.

It's a toy: it may be interesting to read but it's not suitable for any serious use.  I found at least one bug while writing this: there will be many more.

----

Caveat emptor: monsters still lurk in the shadows.

[^1]:	I make no apologies for the pun.

[^2]:	There should be documentation on argument and return conventions, at least: one day.