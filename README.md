## 0. What is Knot?

Knot is a pure λ-calculus reduction engine with optional type inference using
Algorithm J. It implements both untyped and simply-typed (implicitly)
λ-calculus. It translates high-level surface syntax into nameless core terms
using De Bruijn indices, which enables capture-avoiding substitution without
explicit variable renaming during evaluation.

I always wanted to experiment with β-reduction, and building a small reduction
engine sounded like fun. While untyped λ-calculus is neat, I specifically wanted
to implement the simply-typed subsystem (STLC). STLC isn't Turing complete
because it enforces strong normalization; but it is a small proof assistant for
rudimentary propositional logic via the Curry-Howard Isomorphism.

[Miru](https://git.sr.ht/~debarchito/miru) the language I'm currently
researching, has a rather complicated type system; it uses Hindley-Milner at its
core with bidirectional, row-polymorphic and other extensions. This project was
a small (one-day!) experiment to get my hands dirty with Algorithm J and
implementing type systems in general.

## 1. Build and run Knot.

As with most of my projects, you can build Knot using Nix.

```fish
nix build sourcehut:~debarchito/knot#default
./result/bin/knot --help
# or just:
nix run sourcehut:~debarchito/knot#default
```

You can always use dune as a fallback in case you don't use Nix.

```fish
dune build knot --release
dune install
# or just:
dune exec knot --release
```

This will drop you into the Knot REPL. You can also import this repository as a
flake!

```nix
inputs.knot.url = "sourcehut:~debarchito/knot";

nixpkgs.overlays = [
  inputs.knot.overlays.default 
]

environment.systemPackages = [ pkgs.knot ];
```

## 2. Learn Knot in about 5 minutes.

If you want to explore the language by yourself, check out the
[examples](./examples) directory. Feedback and more examples are welcome! This
is how you run them:

```fish
dune exec knot -- examples/1_transitivity.knot
# This example is untyped and needs at-least 800 evaluation steps!
dune exec knot -- examples/2_factorial.knot -u -m 800
```

That said, here's an overview of the language:

```js
// An abstraction that takes one argument and return it. 
\x. x

// Abstractions can also be written using the "fn" keyword.
fn x. x

// Multi-parameter abstractions automatically desugar into nested lambdas.
\x y. x         // Same as \x. \y. x
fn x y z. x z y // Same as \x. \y. \z. (x z) y

// Parentheses are used for grouping sub-expressions.
// These are useful when doing applications.
// NOTE: Applications are left-associative.
(\x. x) (\y. y)

// let-polymorphism in typed mode.
let id = \x. x in id id // Works and outputs \x. x
\id. id id // Occurs check failure (infinite type) in typed mode!

// Application chain using let-polymorphism.
let apply = \f x. f x in apply (\a. a)

// Fixed point operator enables recursive functions in typed mode without
// occurs-check failures. Fix has the signature ('a -> 'a) -> 'a
// NOTE: This function's runtime will scale with the maximum evaluation
// steps set!
let loop = fix (\self. \v. self v) in loop
```

That's all! It's a very small pure functional language; it has no built-in data
types or primitives other than lambdas and the fix operator. This means
everything must be represented using Church encodings (brace yourselves!).

## 3. Licensing.

Knot is made available under the [MIT](./LICENSE) license.
