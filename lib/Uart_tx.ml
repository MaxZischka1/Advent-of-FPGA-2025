open! Core 
open! Hardcaml
open! Signal
open! Always


module State = struct
  type t =
  | Idle
  | Start
  | Transmit
  | Done
  [@@deriving enumerate, compare ~localize, sexp_of]
end

let initilize ~clock ~data ~enable = 

  let spec = Reg_spec.create ~clock () in

  let cpb = 5208 in 

  let cycleCounter = Variable.reg ~width:13 spec in

  let counter = Variable.reg ~width:4 spec in

  let data_reg = Variable.reg ~width:8 spec in

  let outBit = Variable.reg ~width:1 spec in
  let sm = State_machine.create (module State) spec in


  compile[
    sm.switch [
      Idle, [
        outBit <--. 1;
        when_(enable)[
          outBit <--. 0;
          data_reg <-- data;
          if_(cycleCounter.value ==:. cpb)[
            counter <--. 0;
            cycleCounter <--. 0;
            sm.set_next Transmit;
          ][
            cycleCounter <-- cycleCounter.value +:. 1;
          ]
        ];
      ];
      Start,[
        outBit <--. 0;
        if_ (cycleCounter.value ==:. (cpb - 1)) [
          cycleCounter <--. 0;
          counter <--. 0;
          sm.set_next Transmit;
        ] [
          cycleCounter <-- cycleCounter.value +:. 1;
        ]
      ];
      Transmit, [
        outBit <-- select data_reg.value ~high:0 ~low:0;
        if_ (cycleCounter.value ==:. (cpb - 1)) [
          cycleCounter <--. 0;
          data_reg <-- (zero 1 @: (select data_reg.value ~high:7 ~low:1));
          if_ (counter.value ==:. 7) [
            sm.set_next Done;
          ][
            counter <-- counter.value +:. 1;
          ]
        ][
          cycleCounter <-- cycleCounter.value +:. 1;
        ]
      ];
      Done, [
        outBit <--. 1;
        if_ (cycleCounter.value ==:. (cpb - 1)) [
          cycleCounter <--. 0;
          sm.set_next Idle;
        ][
          cycleCounter <-- cycleCounter.value +:. 1;
        ]
      ];
    ];
  ];

  (outBit.value)