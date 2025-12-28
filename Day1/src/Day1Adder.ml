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
    rotSum : 'a[@bits 8];
    counter : 'a[@bits 16];
    dinOut : 'a[@bits 8];
  }[@@deriving hardcaml, sexp_of]
end

module State = struct
  type t = 
    | Idle
    | AddSum
    | AddMod
    | Loop
    | Finish
    [@@deriving enumerate, compare ~localize, sexp_of]
end 

let circuit(i : _ I.t) = 
  let open Signal in

  let open Always in

  let spec = Reg_spec.create ~clock:i.clk ~clear:i.clr () in

  let sm = State_machine.create (module State) spec in

  let acc = Variable.reg ~enable:i.valid ~width:8 spec in

  let addSumAcc = Variable.reg ~enable:i.valid ~width:8 spec in

  let subCounter = Variable.reg ~enable:i.valid ~width:1 spec in

  let counter = Variable.reg ~enable:i.valid ~width:16 spec in

  let dir = Variable.reg ~width:1 spec in

  let () = 
    compile [
      sm.switch[
        Idle, [
          when_ (i.valid)[
            acc <--. 50;
            when_ (i.din ==:. 76) [dir <--. 0; sm.set_next AddSum;];
            when_ (i.din ==:. 82) [dir <--. 1; sm.set_next AddSum;];
          ];
        ];
        AddSum, [
          when_ (i.valid)[
            subCounter <--. 0;
            addSumAcc <-- acc.value;
            when_ (dir.value ==:. 0)[
              when_(acc.value ==:. 0)[counter <-- counter.value -:. 1;];
              acc <-- acc.value -: i.din;
              sm.set_next AddMod;
            ];
            when_ (dir.value ==:. 1)[
              acc <-- acc.value +: i.din;
              sm.set_next AddMod;
            ];
          ];
        ];
        AddMod, [
          when_(i.valid)[
            when_(acc.value >=:. 100)[
              when_(dir.value ==:. 0)[
                acc <-- acc.value +:. 100;
              ];
              when_(dir.value ==:. 1)[
                acc <-- acc.value -:. 100;
              ];
              subCounter <--. 1;
            ];
            when_((acc.value ==:. 0)) [
              subCounter <--. 1;
            ];
            counter <-- (counter.value +: (uresize i.din ~width:16));
            sm.set_next Loop;
          ];
        ];
      Loop, [
        if_(i.valid)[
          counter <-- (counter.value +: (uresize subCounter.value ~width:16));
          when_ (i.din ==:. 76) [dir <--. 0; sm.set_next AddSum; ];
          when_ (i.din ==:. 82) [dir <--. 1; sm.set_next AddSum; ];
        ][
          sm.set_next Finish;
        ]
      ];
      Finish,[

      ];
      ];
    ] in

    


  {O.rotSum = addSumAcc.value; O.counter = counter.value; O.dinOut = i.din}






 