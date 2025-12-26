open! Core
open! Hardcaml
open! DayOne

let main () =
  let module Sim = Cyclesim.With_interface(Day1Adder.I)(Day1Adder.O) in
  let sim_engine = Sim.create Day1Adder.circuit in

  let waves,sim = Hardcaml_waveterm.Waveform.create sim_engine in

  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  inputs.clr := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clr := Bits.gnd;

  let file_input = In_channel.read_all "test/input.txt" in

  let commands = Day1Parser.parse file_input in 
List.iter commands ~f:(fun (dir, valu) ->
    inputs.din := Bits.of_char dir;
    inputs.valid := Bits.vdd;
    Cyclesim.cycle sim;

    inputs.din := Bits.of_int_trunc ~width:8 valu;
    Cyclesim.cycle sim;
    Cyclesim.cycle sim;

    (* 3. Fixed the ! position (must be BEFORE the variable) *)
    Stdio.printf "Full Sum: %d , Count: %d\n"
      (Bits.to_int_trunc !(outputs.rotSum))
      (Bits.to_int_trunc !(outputs.counter))
  );

    Cyclesim.cycle sim;
    Cyclesim.cycle sim;

    (* 2. Print the FINAL state *)
    Stdio.printf "\n--- FINAL HARDWARE STATE ---\n";
    Stdio.printf "Final Sum: %d\n" (Bits.to_int_trunc !(outputs.rotSum));
    Stdio.printf "Final Count: %d\n" (Bits.to_int_trunc !(outputs.counter));
  Stdio.printf "----------------------------\n";
    Hardcaml_waveterm.Waveform.print waves

let () = main ()
