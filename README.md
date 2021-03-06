statmemprof-emacs — Emacs client for statistical memory profiler
-------------------------------------------------------------------------------
%%VERSION%%

statmemprof-emacs is an Sturgeon/emacs front-end of the statmemprof
statistical memory profiler for OCaml.

statmemprof-emacs is distributed under the MIT license.

Homepage: https://github.com/jhjourdan/statmemprof-emacs

## Installation

statmemprof-emacs can be installed with `opam`, when one of the
xxxx-statistical-memprof OCaml switches is installed. These switches
are available on opam. Then you can use the following command:

    opam install statmemprof-emacs

If you don't use `opam` consult the [`opam`](statmemprof-emacs.opam) file for
build instructions.

## Usage

In the OCaml program you need to profile, you can start the profiling
by executing the following instruction (see the documentation in
statmenprof_emacs.mli for more details):

   Statmemprof_emacs.start 1E-4 30 5

Then, in emacs, load the file sturgeon.el (coming with you Sturgeon
installation), and type M-x sturgeon-connect.

Then, you might be asked:
  - for a path for the sturgeon-connector command, which will be in
    your opam swich's ```bin``` directory;
  - a socket to connect to, which emacs auto-complete will help you to
    find.
