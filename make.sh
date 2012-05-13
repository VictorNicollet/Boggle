#!/bin/sh

ocamlopt str.cmxa pairs.ml -o pairs.out
./pairs.out > data.ml
ocamlopt data.ml
ocamlopt data.cmx boggle.ml -o boggle.out
