open! Core
open! Hardcaml
open! DayOnePartOne

let%expect_test "Day1 Simulation" =
  (* 1. Create the Simulator using your Logic's I/O modules *)
  let module Sim = Cyclesim.With_interface (Day1Adder.I) (Day1Adder.O) in
  let sim = Sim.create Day1Adder.circuit in
  
  (* 2. Get the typed input/output records *)
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  (* 3. Function to cycle the clock *)
  inputs.clr := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clr := Bits.gnd;

  (* 4. The Simulation Loop *)
  let lines = In_channel.read_all "input.txt" in
  let commands = DayOnePartOne.Day1Parser.parse lines in

  List.iter commands ~f:(fun (dir,valu) -> 
  inputs.din := Bits.of_char dir;
  inputs.valid := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.din := Bits.of_int_trunc ~width:8 valu;
  Cyclesim.cycle sim;
    );

  Stdio.printf "Final Sum: %d Count: %d, dinOut %d\n" 
  (Bits.to_int_trunc !(outputs.rotSum))
  (Bits.to_int_trunc !(outputs.counter))
  (Bits.to_int_trunc !(outputs.dinOut));
  
  [%expect {| Final Sum: 18 Count: 1031, dinOut 30 |}]