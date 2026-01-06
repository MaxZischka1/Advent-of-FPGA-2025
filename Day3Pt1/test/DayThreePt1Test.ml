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

  let raw_input = {|987654321111110
811111111111119
234234234234278
818181911112111|} in

  let parseVals = DayThreeParser.lineParser2 raw_input in

  List.iter parseVals ~f:(fun(valu) ->
    List.iter valu ~f:(fun(vals) ->
      inputs.valid := Bits.vdd;
      inputs.din := Bits.of_int_trunc ~width:4 vals;
      Cyclesim.cycle sim;
      );

      inputs.valid := Bits.gnd;
      Stdio.printf "Bits:%d\n"
      (Bits.to_int_trunc!(outputs.dout));
      inputs.clr := Bits.vdd;
      Cyclesim.cycle sim;
      inputs.clr := Bits.gnd;
    );

    [%expect {||}]

    

