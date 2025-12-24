open! Core
open! Hardcaml
open! Hardcaml_demo_project

let%expect_test "Advent Day 1 Test" = 
  let module Sim = Cyclesim.With_interface(PipelineLogic.I)(PipelineLogic.O) in

  let sim_engine = Sim.create PipelineLogic.circuit in

  let _waves,sim = Hardcaml_waveterm.Waveform.create sim_engine in

  let inputs = Cyclesim.inputs sim in

  let outputs = Cyclesim.outputs sim in

  inputs.clr := Bits.vdd;
  Cyclesim.cycle sim;
  inputs.clr := Bits.gnd;

  let test_vals = ['L'; '2'; '8';'\n'; 'R' ; '3'; '8';'\n';'R';'4';'0';'\n';'L';'4';'6';'7'] in

  List.iter test_vals ~f:(fun(valu) ->
    inputs.din := Bits.of_char valu;
    inputs.valid := Bits.vdd;
    Cyclesim.cycle sim;
    Stdio.printf "Sum:%d Count: %d \n"
    (Bits.to_int_trunc!(outputs.rotSum))
    (Bits.to_int_trunc!(outputs.counter))
    );

    [%expect.unreachable]