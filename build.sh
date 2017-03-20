#!/bin/bash

cp ./scanner.mll ./scanner.mll
cp ./parser.mly ./parser.mly
cp ./ast.ml ./ast.ml
cp ./sast.ml ./sast.ml
cp ./semant.ml ./semant.ml
cp ./exceptions.ml ./exceptions.ml
cp ./utils.ml ./utils.ml
cp ./codegen.ml ./codegen.ml
cp ./prep.ml ./prep.ml
cp ./neo.ml ./neo.ml

ocamlbuild -j 0 -r -use-ocamlfind -pkgs str,llvm,llvm.analysis,llvm.bitwriter,llvm.bitreader,llvm.linker,llvm.target neo.native
