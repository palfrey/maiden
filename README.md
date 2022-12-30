Maiden
======
[![Continuous integration](https://github.com/palfrey/maiden/actions/workflows/ci.yml/badge.svg?branch=main)](https://github.com/palfrey/maiden/actions/workflows/ci.yml)

Maiden is a [Rockstar](https://github.com/RockstarLang/rockstar) interpreter written in Rust. 

Why the name?
-------------
Well given that Rockstar is about hard rock from the 1980s, [Iron Maiden](https://en.wikipedia.org/wiki/Iron_Maiden) seemed like the obvious name given the existing use of [iron](https://github.com/iron/iron) as a name for Rust programs (despite it actually being named after [a fungus](https://en.wikipedia.org/wiki/Rust_%28fungus%29))

Status
------
Rockstar is still very much in active flux, but we target the [full "fixtures" test suite](https://github.com/RockstarLang/rockstar/tree/master/tests) from the reference implementation (excepting [some issues](https://github.com/RockstarLang/rockstar/issues/168) in it)

Usage
-----
Clone this repo and **update the submodule**
```
git clone https://github.com/palfrey/maiden.git
cd maiden
git submodule update --init
```
After this, `cargo run --quiet <your rockstar program>` works pretty well

Web version
-----------
There's a deployed edition at https://palfrey.github.io/maiden/. To work with it
1. Install [trunk](https://trunkrs.dev/)
2. Run `trunk serve --release` (weird bugs with debug config for some reasons)
3. Goto http://localhost:8080/
