Maiden
======
[![Build Status](https://travis-ci.org/palfrey/maiden.svg?branch=master)](https://travis-ci.org/palfrey/maiden)

Maiden is a [Rockstar](https://github.com/dylanbeattie/rockstar) interpreter written in Rust. 

Why the name?
-------------
Well given that Rockstar is about hard rock from the 1980s, [Iron Maiden](https://en.wikipedia.org/wiki/Iron_Maiden) seemed like the obvious name given the existing use of [iron](http://ironframework.io/) as a name for Rust programs (despite it actually being named after [a fungus](https://en.wikipedia.org/wiki/Rust_%28fungus%29))

Status
------
Rockstar is still very much in active flux, but we target the [full "correct" test suite](https://github.com/dylanbeattie/rockstar/tree/reference-implementation/tests/correct) from the reference implementation (excepting [some issues](https://github.com/dylanbeattie/rockstar/issues/168) in it)

Usage
-----
`cargo run --quiet <your rockstar program>` works pretty well

Web version
-----------
There's a deployed edition at https://palfrey.github.io/maiden/. To work with it
1. [Install cargo web](https://github.com/koute/cargo-web#installation)
2. Run `cargo web start --auto-reload`
3. Goto http://localhost:8000/