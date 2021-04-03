Maiden
======
[![Continuous integration](https://github.com/palfrey/maiden/actions/workflows/ci.yml/badge.svg)](https://github.com/palfrey/maiden/actions/workflows/ci.yml)

Maiden is a [Rockstar](https://github.com/dylanbeattie/rockstar) interpreter written in Rust. 

Why the name?
-------------
Well given that Rockstar is about hard rock from the 1980s, [Iron Maiden](https://en.wikipedia.org/wiki/Iron_Maiden) seemed like the obvious name given the existing use of [iron](https://github.com/iron/iron) as a name for Rust programs (despite it actually being named after [a fungus](https://en.wikipedia.org/wiki/Rust_%28fungus%29))

Status
------
Rockstar is still very much in active flux, but we target the [full "fixtures" test suite](https://github.com/RockstarLang/rockstar/tree/master/tests) from the reference implementation (excepting [some issues](https://github.com/dylanbeattie/rockstar/issues/168) in it)

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
1. [Install cargo web](https://github.com/koute/cargo-web#installation) (0.6.24 or above, because of stdweb)
2. Run `cargo web start --auto-reload --release` (weird bugs with debug config for some reasons)
3. Goto http://localhost:8000/
