#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "statmemprof-emacs" @@ fun c ->
  Ok [ Pkg.mllib "src/statmemprof_emacs.mllib"; ]
