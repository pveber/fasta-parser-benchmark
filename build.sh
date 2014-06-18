ocamlbuild -use-ocamlfind -pkgs threads,biocaml,batteries,lwt.unix,lwt.syntax -syntax camlp4o -tag thread -tag annot -tag debug main.native
