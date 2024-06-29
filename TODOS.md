# TODOS

## FIXME

 - [src/frontend.rs#L589](src/frontend.rs#L589) @mhs:  should find a better way to handle this cases and exit gracefully
 - [src/frontend.rs#L570](src/frontend.rs#L570) @mhs:  what to do if we have an infinite type definition / recursive type like a binary tree?

## TEST

 - [src/frontend.rs#L89](src/frontend.rs#L89) @mhs:  asnasnt

## TODO

 - [src/frontend.rs#L209](src/frontend.rs#L209) @mhs:  decide which algorithm for type inference works best for Goburin, W or M.
 - [src/frontend.rs#L557](src/frontend.rs#L557) @mhs:  as far as I understood, this `MonoType` limitation is what makes this implementation
 - [src/frontend.rs#L749](src/frontend.rs#L749) @ :  improve errors
 - [src/frontend.rs#L800](src/frontend.rs#L800) @ :  improve errors
 - [src/frontend.rs#L775](src/frontend.rs#L775) @ :  improve errors
 - [src/frontend.rs#L838](src/frontend.rs#L838) @mhs:  shamelessly stolen from odin-lang at (https://odin-lang.org/docs/overview/#operator-precedence)
 - [src/frontend.rs#L851](src/frontend.rs#L851) @ :  make a single function for the binding powers
 - [src/frontend.rs#L871](src/frontend.rs#L871) @mhs:  is equal right associative?
 - [src/frontend.rs#L1019](src/frontend.rs#L1019) @mhs:  a tab counts as 4 columns, for now
 - [src/middleend.rs#L124](src/middleend.rs#L124) @mhs:  for now memory is "zero-initialized"
 - [src/middleend.rs#L108](src/middleend.rs#L108) @mhs:  for now "let" and "mut" are basically the same
 - [src/middleend.rs#L266](src/middleend.rs#L266) @mhs:  improve this, making it dumb so it is easy to self-host later

