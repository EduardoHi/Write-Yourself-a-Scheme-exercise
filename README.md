
# Write Yourself a Scheme Exercise

## Introduction
this repo contains all the files I created while following the awesome tutorial of [Write Yourself a Scheme in 48 hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours) although I gotta admit (and you can see in the commit history) it took me far more than 48 hours. It makes me wonder wether it was 48 person hours instead of calendar hours :p. Nevertheless, If you haven't read it, it's an excellent resource to learn about haskell if you already now lisp, or learn about scheme if you already know haskell.

## Differences vs. the tutorial

Some of the code is simply taken from the tutorial, although I manually rewrote almost all code, to work myself through the machinery.

The rest of the code are extra things I wanted to implement, things I changed, and a bunch of the exercises interleaved in the tutorial, a non exhaustive list of things I changed:
- Used Megaparsec instead of Parsec
- Implemented proper string literals, with character escaping.
- Support for oct and hex literals
- Added the full Scheme numeric tower, although I missed some of the polymorphism
- Implemented backquote: quasiquote and quote
- added type testing functions e.g. `string?`, also removed some of the type coercion, although standard schemes lets it
- symbol-handling functions e.g `string->symbol`
- implemented cond, (previously, there was case too, but it was removed when the `Func` in `LispVal` was added, because `Eq` is undefined for functions)


## How to run

This is assuming you have `ghc` installed (I have version `8.0.1` but probably other versions work too).

compiling: `ghc -main-is SchemeRepl -o my_scheme SchemeRepl.hs`

there are two ways of using it:
- starting a repl: `./my_scheme` running the program without arguments. To exit the Repl use ctrl-D or write `quit`
- batch mode: `./my_scheme file1.scm` the first argument is the file name, the rest of the arguments are binded to variable `args` inside the program

(there is a third way actually, running `runhaskell SchemeRepl.hs` this has the advantage of not generating the `*.hi` and `*.o` files, but it compiles everytime.


# Standard Library:

_note: both the primitives and the standard library should behave just like the scheme standard specifies_

- boolean
  - `not`

- functions
  - `id`
  - `flip`
  - `curry`
  - `compose`

- number predicates
  - `zero?`
  - `positive?`
  - `negative?`
  - `odd?`
  - `even?`

- lists
  - `null?`
  - `list`
  - `foldr`
  - `foldl`
  - `fold`
  - `reduce`
  - `unfold`
  - `sum`
  - `product`
  - `and`
  - `or`
  - `max`
  - `min`
  - `length`
  - `reverse`
  - `memq`
  - `memv`
  - `member`
  - `assq`
  - `asv`
  - `assoc`
  - `map`
  - `filter`

# Primitive functions:

- arithmetic
  - `+`
  - `-`
  - `*`
  - `/`
  - `mod`
  - `quotient`
  - `remainder`
- numeric boolean
  - `=`
  - `<`
  - `>`
  - `/=`
  - `>=`
  - `<=`
- string boolean
  - `string=?`
  - `string<?`
  - `string>?`
  - `string<=?`
  - `string>=?`
- boolean
  - `&&`
  - `||`
- lists
  - `car`
  - `cdr`
  - `cons`
- equality
  - `eq?`
  - `eqv?`
  - `equal?`
- type tests
  - `boolean?`
  - `pair?`
  - `vector?`
  - `number?`
  - `string?`
  - `char?`
  - `symbol?`
- conversions
  - `symbol->string`
  - `string->symbol`
- IO Primitives
  - `apply`
  - `open-input-file`
  - `open-output-file`
  - `close-input-port`
  - `close-output-port`
  - `read`
  - `write`
  - `read-contents`
  - `read-all`

# Special Forms

- `quote`
- `if`
- `cond`
- `set!`
- `define`
- `lambda`
- `load`
