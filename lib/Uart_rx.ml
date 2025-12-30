open! Core
open! Hardcaml
open! Signal
open! Always

module States = struct
  type t =
  | Idle
  | Start
  | Data
  | Finish
    [@@deriving enumerate, compare ~localize, sexp_of]
  end 

  let initialize ~clock ~rx =
    let spec = Reg_spec.create ~clock () in
    let cpb = 4 in (*fCK/115200*)
    let midCPB = 2 in (*Take sample on middle of signal*)

    let cycleCount = Variable.reg ~width:9 spec in
    let bitCount = Variable.reg ~width:4 spec in
    let data = Variable.reg ~width:8 spec in
    let valid = Variable.reg ~width:1 spec in (*Change this in SM*)

    let sm = State_machine.create (module States) spec in

    compile [
      sm.switch [
        Idle,[
          when_(rx ==:. 0)[
            cycleCount <--. 0;
            valid <--. 0;
            sm.set_next Start;
          ];
        ];
        Start, [
          if_(cycleCount.value ==:. midCPB)[
            cycleCount <--. 0;
            sm.set_next Data;
          ][
            cycleCount <-- cycleCount.value +:. 1;
          ]
        ];
        Data, [
          if_(cycleCount.value ==:. (cpb - 1))[
            cycleCount <--. 0;
            data <-- (rx @: (select ~high:7 ~low:1 data.value));
            if_(bitCount.value ==:. 7)[
              bitCount <--. 0;
              sm.set_next Finish;
              valid <--. 1;
            ][
              bitCount <-- bitCount.value +:. 1;
            ]
          ][
            cycleCount <-- cycleCount.value +:. 1;
          ]
        ];
        Finish, [
          if_(cycleCount.value ==:. (cpb - 1))[
            cycleCount <--. 0;
            valid <--. 0;
            sm.set_next Idle;
          ][
            cycleCount <-- cycleCount.value +:. 1;
          ]
        ];
      ]
    ];

    (data.value, valid.value)
