(lang dune 2.9)
(name 6502-ml)

; (version 1.0.0)

(generate_opam_files true)

(source (github Firobe/6502-ml))
(license MIT)
(authors "Virgile Robles")
(maintainers "virgile.robles@pm.me")

(package
 (name 6502-ml)
 (synopsis "Library for the simulation 6502 chip family")
 (description "Library for the simulation 6502 chip family")
 (depends
  stdint
  stdint-literals
  (dune :build)
  (re :with-test)
  (str :wth-test)))
