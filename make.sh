#!/bin/sh

ocamlopt str.cmxa pairs.ml -o pairs.out
./pairs.out > data.ml
ocamlopt data.ml
ocamlopt str.cmxa unix.cmxa data.cmx boggle.ml -o boggle.out
