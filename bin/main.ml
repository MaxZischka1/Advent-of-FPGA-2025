open! Core
open! Hardcaml
open! DayOne
open! DayOneWrapper

module Sim = Cyclesim.With_interface(Day1Wrap.I)(Day1Wrap.O)

let run_simulation () =
  let sim_engine = Sim.create Day1Wrap.circuit in
  let vcd_file = Out_channel.create "waveform.vcd" in

  Exn.protect ~f:(fun () ->
    
    let sim = Hardcaml.Vcd.wrap vcd_file sim_engine in
    let inputs = Cyclesim.inputs sim in
    
  let sendInt (valu : int) = (*function to make List.iter much more readable*)
    inputs.rx := Bits.gnd;
    for _ = 1 to 4 do Cyclesim.cycle sim done;
    for i = 0 to 7 do
      inputs.rx := if((((valu lsr i) land 1)) = 1) then Bits.vdd else Bits.gnd;
      for _ = 1 to 4 do Cyclesim.cycle sim done;
    done;
    inputs.rx := Bits.vdd;
    for _ = 1 to 7 do Cyclesim.cycle sim done;
  in

  inputs.clr := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clr := Bits.gnd;
  Cyclesim.cycle sim;
  inputs.rx := Bits.vdd;
  Cyclesim.cycle sim;

  let lines = In_channel.read_all "Day1/test/input.txt" in

  let commands = Day1Parser.parse lines in

  List.iter commands ~f:(fun(dir,valu, hundVal) ->  (*iterates through all values in list*)
    sendInt(dir);
    for _ = 1 to 7 do Cyclesim.cycle sim done;
    sendInt(valu);
    for _ = 1 to 7 do Cyclesim.cycle sim done;
    sendInt(hundVal);
    for _ = 1 to 7 do Cyclesim.cycle sim done;
  );
  sendInt(240);
  for _ = 1 to 7 do Cyclesim.cycle sim done;
  sendInt(0);
  for _ = 1 to 7 do Cyclesim.cycle sim done;


    ) ~finally:(fun () -> (*Had gemini help me create vcd files*)
      Out_channel.close vcd_file
      )

  let () = run_simulation ()