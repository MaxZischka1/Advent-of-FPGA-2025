open! Core (*Didn't use waveform of waveterm need don't really understand it*)
open! Hardcaml
open! DayOne

let%expect_test "Advent of Code TB" = 
  let module Sim = Cyclesim.With_interface(Day1Adder.I)(Day1Adder.O) in
  let sim = Sim.create Day1Adder.circuit in
  (*let _waves,sim = Hardcaml_waveterm.Waveform.create sim_engine in*)

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
  (*test for not valid data bits*)
  List.iter commands ~f:(fun(dir,valu, hundVal) ->
    inputs.valid := Bits.gnd;
    Cyclesim.cycle sim;
    inputs.din := Bits.of_int_trunc ~width:8 dir;
    Cyclesim.cycle sim;
    inputs.valid := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.din := Bits.of_int_trunc ~width:8 valu;
    Cyclesim.cycle sim;
    inputs.din := Bits.of_int_trunc ~width:8 hundVal;
    inputs.valid := Bits.gnd;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;
    inputs.valid := Bits.vdd;
    Cyclesim.cycle sim;
    Stdio.printf "Count: %d\n"
    (*(Bits.to_int_trunc !(outputs.rotSum)) *)
    (Bits.to_int_trunc !(outputs.counter));
    );
    Cyclesim.cycle sim;

    Stdio.printf "Count: %d\n"
    (* (Bits.to_int_trunc !(outputs.rotSum)) *)
    (Bits.to_int_trunc !(outputs.counter));
    (* (Bits.to_int_trunc !(outputs.dinOut)) *)
    [%expect {||}]