(lang dune 3.24)

(name knot)

(generate_opam_files true)

(cram enable)

(source
 (sourcehut debarchito/knot))

(authors "Debarchito Nath <debarchiton@proton.me>")

(maintainers "Debarchito Nath <debarchiton@proton.me>")

(package
 (name knot)
 (synopsis "A pure λ-calculus reduction engine with optional type inference")
 (description
  "Knot is a pure λ-calculus reduction engine with optional type inference using Algorithm J")
 (depends
  (ocaml
   (>= "5.5.0")))
 (tags ("knot")))
