open! Core
open! Hardcaml
module I = struct
  type 'a t = {
    clk : 'a;
    clr : 'a;
    valid : 'a;
    din : 'a[@bits 8];
  }[@@deriving hardcaml, sexp_of]
end

module O = struct
  type 'a t = {
    rotSum : 'a[@bits 7];
    counter : 'a[@bits 8];
  }[@@deriving hardcaml, sexp_of]
end

module State = struct
  type t = 
    | Init
    | Idle
    | Left
    | Right
    | Add10
    | Sub10
    | Add1
    | Sub1
    | FinishedAdd
    | FinishedSub
    [@@deriving enumerate, compare ~localize, sexp_of]
end

let circuit(i : _ I.t) = 
  let open Signal in
  let open Always in

  let spec = Reg_spec.create ~clock:i.clk ~clear:i.clr () in

  let acc = Variable.reg ~enable:i.valid ~width:8 spec in

  let sm = State_machine.create (module State) spec in

  let tempVal = Variable.reg spec ~width:8 in
  let newSum = Variable.reg spec ~width:8 in
  let counter = Variable.reg spec ~width:8 in

  let input_digit = i.din -: Signal.of_int_trunc ~width:8 48 in

  let times_10 = (sll input_digit ~by:3) +: (sll input_digit ~by:1) in

  let () = 
    compile [
      if_(sm.is Init)[
        acc <--. 50;
        if_ (i.valid)[
         switch i.din[
          Signal.of_int_trunc ~width:8 76, [ sm.set_next Sub10 ]; (* 'L' *)
          Signal.of_int_trunc ~width:8 82, [ sm.set_next Add10 ]; (* 'R' *)
         ]
        ][];
      ][
      if_(sm.is Idle)[
        tempVal <--. 0;
        if_(acc.value ==:. 0)[
          counter <-- counter.value +:. 1;
        ][];
        if_ (i.valid)[
         switch i.din[
          Signal.of_int_trunc ~width:8 76, [ sm.set_next Sub10 ]; (* 'L' *)
          Signal.of_int_trunc ~width:8 82, [ sm.set_next Add10 ]; (* 'R' *)
         ]
        ][];
      ][ if_(sm.is Add10)[
        if_ (i.valid)[
          tempVal <-- times_10;
          sm.set_next Add1;
        ][];
      ][
        if_(sm.is Sub10)[
          if_(i.valid)[
            tempVal <-- times_10;
            sm.set_next Sub1;
          ][];
        ][if_(sm.is Add1)[
          if_(i.valid)[
            newSum <-- acc.value +: (tempVal.value +: input_digit);
            sm.set_next FinishedAdd;
          ][];
        ][
          if_(sm.is Sub1)[
            if_(i.valid)[
              newSum <-- acc.value -: (tempVal.value +: input_digit);
              sm.set_next FinishedSub;
            ][];
          ][
            if_(sm.is FinishedAdd)[
            if_(newSum.value >=:. 100)[
              acc <-- newSum.value -: Signal.of_int_trunc ~width:8 100;
            ][
              acc <-- newSum.value;
            ];
            if_(i.din ==: Signal.of_int_trunc ~width:8 10)[
                sm.set_next Idle;
              ][];
            ][
              if_(sm.is FinishedSub)[
              if_(newSum.value >=:. 128)[
                acc <-- newSum.value +: Signal.of_int_trunc ~width:8 100;
              ][
                acc <-- newSum.value;
              ];
              if_(i.din ==: Signal.of_int_trunc ~width:8 10)[
                sm.set_next Idle;
              ][];
              ][];
            ];
          ];
        ];
        ];
      ];
      ];
      ];
    ] in

    let acc7 = uresize acc.value ~width:7 in

    {O.rotSum = acc7; O.counter = counter.value}

(*First Iteration WORKINGINGING*)