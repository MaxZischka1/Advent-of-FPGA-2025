open! Core
open! Hardcaml
open! DayOnePL

let%expect_test "PLTestBench" =
  Stdio.print_endline "--- DEBUG: TEST STARTED ---";  
  let module Sim = Cyclesim.With_interface(Day1PLAdder.I)(Day1PLAdder.O) in

  let sim_engine = Sim.create Day1PLAdder.circuit in

  let _waves,sim = Hardcaml_waveterm.Waveform.create sim_engine in

  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

inputs.clr := Bits.vdd;
Cyclesim.cycle sim;
inputs.clr := Bits.gnd;

let raw_input = {|R28
R38
R40
L467|} in
  let commands = Day1PLParser.parse raw_input in 

  List.iter commands ~f:(fun(charV, valu, hundVal) ->
  inputs.valid := Bits.vdd;
  inputs.char := Bits.of_int_trunc ~width:8 charV;
  inputs.din := Bits.of_int_trunc ~width:8 valu;
  inputs.hundVal := Bits.of_int_trunc ~width:8 hundVal;
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
