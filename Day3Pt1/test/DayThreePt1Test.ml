open! Core
open! Hardcaml
open! DayThreePartOne

let%expect_test "Day Three Simple test" =
  let module Sim = Cyclesim.With_interface(DayThreeComparator.I)(DayThreeComparator.O) in
  let sim = Sim.create DayThreeComparator.circuit in

  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in

  inputs.clr := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clr := Bits.gnd;

  let lines = In_channel.read_all "inputs.txt" in

  let parseVals = DayThreeParser.lineParser2 lines in

  List.iter parseVals ~f:(fun(valu) ->
    List.iter valu ~f:(fun(vals) ->
      inputs.valid := Bits.vdd;
      inputs.din := Bits.of_int_trunc ~width:4 vals;
      Cyclesim.cycle sim;
      );
    inputs.valid := Bits.gnd;
      Cyclesim.cycle sim;
      Stdio.printf "Bits:%d\n"
      (Bits.to_int_trunc!(outputs.dout));
      
      );
      inputs.valid := Bits.vdd;
      Cyclesim.cycle sim;
      inputs.valid := Bits.gnd;
      Cyclesim.cycle sim;
      Stdio.printf "Bitsy:%d\n"
      (Bits.to_int_trunc!(outputs.dout));

    [%expect {||}]

    

