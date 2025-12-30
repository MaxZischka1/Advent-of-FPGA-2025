open! Core
open! Hardcaml
open! DayOneWrapper

module Sim = Cyclesim.With_interface(Day1Wrap.I)(Day1Wrap.O)

let run_test() = 
  let sim = Sim.create Day1Wrap.circuit in
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  let send_bit b = 
    inputs.rx := if (b = 1) then Bits.vdd else Bits.gnd;
    for _ = 1 to 434 do Cyclesim.cycle sim done
  in

  inputs.clr := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clr := Bits.gnd;
  inputs.rx := Bits.vdd;
  for _ = 1 to 100 do Cyclesim.cycle sim done;

  send_bit 0;

  let sigs = [0; 1; 0; 0; 1; 1; 0; 0] in (*'L'*)

  List.iter sigs ~f:send_bit;

  send_bit 1;
  for _ = 1 to 600 do Cyclesim.cycle sim done;

  (* send_bit 0;

  let sigs = [0; 0; 1; 0; 0; 0; 0; 1] in (*'33'*)

  List.iter sigs ~f:send_bit;

  send_bit 1;
  for _ = 1 to 600 do Cyclesim.cycle sim done;

  send_bit 0;

  let sigs = [0; 0; 0; 0; 0; 0; 0; 1] in (*'100'*)

  List.iter sigs ~f:send_bit;

  send_bit 1;
  for _ = 1 to 600 do Cyclesim.cycle sim done; *)

  
  Stdio.printf "Count: %d curSum:%d dinOut:%d\n" 
  (Bits.to_int_trunc !(outputs.count))
  (Bits.to_int_trunc !(outputs.curSum))
  (Bits.to_int_trunc !(outputs.dinOut));
  ()

let () = run_test ()
