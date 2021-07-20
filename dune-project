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
 (synopsis "An OCaml library for HTTP clients and servers")
 (description "A longer description")
 (depends
  (stdint (>= 0.7.0))
  (dune (>= 2.8.0))
  (re :with-test)
  (str :wth-test)))
