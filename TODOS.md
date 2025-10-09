# TODOS

## NOTE

 - [tiny_goburin/src/compiler/p1_0_parse.rs#L73](tiny_goburin/src/compiler/p1_0_parse.rs#L73) @mhs:  for now we only support function bodies
 - [tiny_goburin/src/compiler/p1_0_parse.rs#L96](tiny_goburin/src/compiler/p1_0_parse.rs#L96) @mhs:  do not allow empty function bodies

## FIXME

 - [tiny_goburin/src/compiler/compiler.rs#L279](tiny_goburin/src/compiler/compiler.rs#L279) @mhs:  remove the symbol in here
 - [tiny_goburin/src/compiler/compiler.rs#L409](tiny_goburin/src/compiler/compiler.rs#L409) @mhs:  cannot create "Array [10] Integer", just "Array Integer"
 - [goburin/src/frontend.rs#L507](goburin/src/frontend.rs#L507) @mhs:  what to do if we have an infinite type definition / recursive type like a binary tree?
 - [goburin/src/frontend.rs#L526](goburin/src/frontend.rs#L526) @mhs:  should find a better way to handle this cases and exit gracefully

## TODO

 - [tiny_goburin/src/compiler/p1_2_static_type_checking.rs#L49](tiny_goburin/src/compiler/p1_2_static_type_checking.rs#L49) @mhs:  Allow for arbitrary return types
 - [tiny_goburin/src/compiler/p1_2_static_type_checking.rs#L138](tiny_goburin/src/compiler/p1_2_static_type_checking.rs#L138) @mhs:  possibly change this to the "Never" type to indicate that this can never happen?
 - [tiny_goburin/src/compiler/p0_0_tokenize.rs#L45](tiny_goburin/src/compiler/p0_0_tokenize.rs#L45) @mhs:  a tab counts as 4 columns, for now
 - [tiny_goburin/src/compiler/p1_0_parse.rs#L362](tiny_goburin/src/compiler/p1_0_parse.rs#L362) @mhs:  is equal right associative?
 - [tiny_goburin/src/compiler/p1_0_parse.rs#L342](tiny_goburin/src/compiler/p1_0_parse.rs#L342) :  make a single function for the binding powers
 - [tiny_goburin/src/compiler/p1_0_parse.rs#L329](tiny_goburin/src/compiler/p1_0_parse.rs#L329) @mhs:  shamelessly stolen from odin-lang at (https://odin-lang.org/docs/overview/#operator-precedence)
 - [tiny_goburin/src/compiler/p1_0_parse.rs#L418](tiny_goburin/src/compiler/p1_0_parse.rs#L418) @mhs:  add the rest of recursive/multi types
 - [tiny_goburin/src/compiler/p1_0_parse.rs#L473](tiny_goburin/src/compiler/p1_0_parse.rs#L473) @mhs:  keep the Token<_> struct for better error messages
 - [tiny_goburin/src/compiler/p1_0_parse.rs#L501](tiny_goburin/src/compiler/p1_0_parse.rs#L501) @mhs:  for now we only accept single types as return values
 - [tiny_goburin/src/compiler/p1_0_parse.rs#L459](tiny_goburin/src/compiler/p1_0_parse.rs#L459) @mhs:  keep the Token<_> struct for better error messages
 - [tiny_goburin/src/compiler/p1_0_parse.rs#L447](tiny_goburin/src/compiler/p1_0_parse.rs#L447) @mhs:  most simple way to have 4 states, once we are self-hosted I should improve this mess
 - [tiny_goburin/src/compiler/compiler.rs#L74](tiny_goburin/src/compiler/compiler.rs#L74) @mhs:  move this to a "common"/"shared" module
 - [tiny_goburin/src/compiler/compiler.rs#L81](tiny_goburin/src/compiler/compiler.rs#L81) @mhs:  improve this, making it dumb so it is easy to self-host later
 - [tiny_goburin/src/compiler/compiler.rs#L235](tiny_goburin/src/compiler/compiler.rs#L235) @mhs:  what is the `name` used for? I think it is for symbol lookup
 - [tiny_goburin/src/compiler/compiler.rs#L465](tiny_goburin/src/compiler/compiler.rs#L465) @mhs:  Remove this
 - [gbvm/src/lib.rs#L1](gbvm/src/lib.rs#L1) @mhs:  keep it in early development
 - [gbvm/src/machine.rs#L126](gbvm/src/machine.rs#L126) @mhs:  handle over/under-flowing case
 - [gbvm/src/machine.rs#L133](gbvm/src/machine.rs#L133) @mhs:  handle over/under-flowing case
 - [gbvm/src/machine.rs#L141](gbvm/src/machine.rs#L141) @mhs:  handle over/under-flowing case
 - [gbvm/src/machine.rs#L165](gbvm/src/machine.rs#L165) @mhs:  handle over/under-flowing case
 - [gbvm/src/machine.rs#L193](gbvm/src/machine.rs#L193) @mhs:  handle over/under-flowing case
 - [gbvm/src/machine.rs#L201](gbvm/src/machine.rs#L201) @mhs:  handle over/under-flowing case
 - [gbvm/src/machine.rs#L172](gbvm/src/machine.rs#L172) @mhs:  handle over/under-flowing case
 - [goburin/src/middleend.rs#L110](goburin/src/middleend.rs#L110) @mhs:  for now "let" and "mut" are basically the same
 - [goburin/src/middleend.rs#L134](goburin/src/middleend.rs#L134) @mhs:  for now memory is "zero-initialized"
 - [goburin/src/middleend.rs#L286](goburin/src/middleend.rs#L286) @mhs:  improve this, making it dumb so it is easy to self-host later
 - [goburin/src/middleend.rs#L284](goburin/src/middleend.rs#L284) @mhs:  improve this, making it dumb so it is easy to self-host later
 - [goburin/src/frontend.rs#L146](goburin/src/frontend.rs#L146) @mhs:  decide which algorithm for type inference works best for Goburin, W or M.
 - [goburin/src/frontend.rs#L494](goburin/src/frontend.rs#L494) @mhs:  as far as I understood, this `MonoType` limitation is what makes this implementation
 - [goburin/src/frontend.rs#L706](goburin/src/frontend.rs#L706) :  improve errors
 - [goburin/src/frontend.rs#L758](goburin/src/frontend.rs#L758) :  improve errors
 - [goburin/src/frontend.rs#L732](goburin/src/frontend.rs#L732) :  improve errors
 - [goburin/src/frontend.rs#L796](goburin/src/frontend.rs#L796) @mhs:  shamelessly stolen from odin-lang at (https://odin-lang.org/docs/overview/#operator-precedence)
 - [goburin/src/frontend.rs#L829](goburin/src/frontend.rs#L829) @mhs:  is equal right associative?
 - [goburin/src/frontend.rs#L809](goburin/src/frontend.rs#L809) :  make a single function for the binding powers
 - [goburin/src/frontend.rs#L986](goburin/src/frontend.rs#L986) @mhs:  a tab counts as 4 columns, for now

