#!/bin/bash
cd src
ocamlopt -o tests Unrolled.mli Unrolled.ml UnrolledTests.ml
./tests
cd ..
ocamldoc -html -d docs/ src/Unrolled.mli
