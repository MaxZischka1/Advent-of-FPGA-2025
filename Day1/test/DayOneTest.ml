open! Core
open! Hardcaml
open! DayOne

let%expect_test "Advent of Code TB" = 
  let module Sim = Cyclesim.With_interface(Day1Adder.I)(Day1Adder.O) in
  let sim_engine = Sim.create Day1Adder.circuit in
  let _waves,sim = Hardcaml_waveterm.Waveform.create sim_engine in

  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  inputs.clr := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clr := Bits.gnd;

  let lines = In_channel.read_all "input.txt" in
  let commands = Day1Parser.parse lines in 
  List.iter commands ~f:(fun(dir,valu, hundVal) ->
    inputs.din := Bits.of_char dir;
    inputs.valid := Bits.vdd;
    Cyclesim.cycle sim;
    inputs.din := Bits.of_int_trunc ~width:8 valu;
    Cyclesim.cycle sim;
    inputs.din := Bits.of_int_trunc ~width:8 hundVal;
    Cyclesim.cycle sim;
    );
    Cyclesim.cycle sim;

    Stdio.printf "Full Sum: %d , Count: %d, dinOut: %d\n"
    (Bits.to_int_trunc !(outputs.rotSum))
    (Bits.to_int_trunc !(outputs.counter))
    (Bits.to_int_trunc !(outputs.dinOut));
    [%expect {| Full Sum: 18 , Count: 5831, dinOut: 0 |}]