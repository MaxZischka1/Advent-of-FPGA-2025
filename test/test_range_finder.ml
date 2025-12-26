open! Core
open! Hardcaml
open! Hardcaml_demo_project

let%expect_test "Advent of Code TB" = 
  let module Sim = Cyclesim.With_interface(Day1Adder.I)(Day1Adder.O) in
  let sim_engine = Sim.create Day1Adder.circuit in
  let _waves,sim = Hardcaml_waveterm.Waveform.create sim_engine in

  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

inputs.clr := Bits.vdd;
Cyclesim.cycle sim;
inputs.clr := Bits.gnd;

let raw_input = {|L28
R38
R40
L467|} in
  let commands = Day1Parser.parse raw_input in 
  List.iter commands ~f:(fun(dir,valu) ->
  inputs.din := Bits.of_char dir;
    inputs.valid := Bits.vdd;

    Cyclesim.cycle sim;

  inputs.din := Bits.of_int_trunc ~width:8 valu;

    Cyclesim.cycle sim;

    Cyclesim.cycle sim;

    Stdio.printf "Full Sum: %d , Count: %d\n"
    (Bits.to_int_trunc!(outputs.rotSum))
    (Bits.to_int_trunc!(outputs.counter))
    );

    [%expect {|
    Processed L28 -> Sum:78 Count: 0 
    Processed R38 -> Sum:40 Count: 0 
    Processed R40 -> Sum:0 Count: 1 
    Processed L467 -> Sum:67 Count: 5 |}]