Final Project: OCalc
ask282, jb2354, kl639
Follow the steps below for running the code on Mac. Unfortunately the graphics do not work on Windows without a Virtual Machine.

Installation steps:
If you do not have OCaml’s Graphics module:
Download XQuartz: https://www.xquartz.org/
Run the command “opam switch reinstall 4.05.0”
If you do not have OUnit installed
Run the command “opam install ounit”

Run steps:
To build and launch the calculator, run the command “make repl”. Starting with the “help” command may be useful.
To run the test suite, run the command “make test”
