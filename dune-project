(lang dune 3.16)

(name cs3110finalproject)

(generate_opam_files true)

(source
 (github tomas-salgado/cs3110-final-project))

(authors "Tomas Salgado" "Vanessa Morales" "Tim Holt")

(maintainers "Tomas Salgado" "Vanessa Morales" "Tim Holt")

(license MIT)

(documentation https://github.com/tomas-salgado/cs3110-final-project)

(package
 (name cs3110finalproject)
 (synopsis "Camelot Adventures - An Interactive Choose Your Own Adventure Game")
 (description "An interactive choose your own adventure game where players select a character and make choices to save Camelot from invasion. Players can choose from different characters with unique abilities and must navigate through various scenarios while managing their health, food, and gold.")
 (depends ocaml dune)
 (tags
  (topics "game" "interactive" "adventure" "camelot")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
