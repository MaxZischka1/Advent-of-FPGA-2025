open! Core
open! Hardcaml
module I = struct
  type 'a t = {
    clk : 'a;
    clr : 'a;
    valid : 'a;
    din : 'a[@bits 8]; (*We will create an input of a char 'L' or 'R' then an in between 0 and 99*)
  }[@@deriving hardcaml, sexp_of]
end

module O = struct
  type 'a t = {
    rotSum : 'a[@bits 8];
    counter : 'a[@bits 8];
  }[@@deriving hardcaml, sexp_of]
end

module State = struct
  type t = 
    | Idle
    | AddSum
    | Eval
    [@@deriving enumerate, compare ~localize, sexp_of]
end 

let circuit(i : _ I.t) = 
  let open Signal in

  let open Always in

  let spec = Reg_spec.create ~clock:i.clk ~clear:i.clr () in

  let sm = State_machine.create (module State) spec in

  let acc = Variable.reg ~enable:i.valid ~width:8 spec in

  let counter = Variable.reg ~enable:i.valid ~width:8 spec in

  let dir = Variable.reg ~width:1 spec in

  let () = 
    compile [
      sm.switch[
        Idle, [
          when_ (i.valid)[
            acc <--. 50;
            when_ (i.din ==:. 76) [dir <--. 0; sm.set_next AddSum; ];
            when_ (i.din ==:. 82) [dir <--. 1; sm.set_next AddSum; ];
          ];
        ];
        AddSum, [
          when_ (i.valid)[
            when_(acc.value ==:. 0)[
              counter <-- counter.value +:. 1;
            ];
            when_ (dir.value ==:. 0)[
              acc <-- acc.value -: i.din;
              sm.set_next Eval;
            ];
            when_ (dir.value ==:. 1)[
              acc <-- acc.value +: i.din;
              sm.set_next Eval;
            ];
          ];
        ];
        Eval, [
          when_(i.valid)[
            when_(acc.value >=:. 100)[
              when_(dir.value ==:. 0)[
                acc <-- acc.value +:. 100;
              ];
              when_(dir.value ==:. 1)[
                acc <-- acc.value -:. 100;
              ];
            ];
            when_ (i.din ==:. 76) [dir <--. 0; sm.set_next AddSum; ];
            when_ (i.din ==:. 82) [dir <--. 1; sm.set_next AddSum; ];
          ];
        ];
      ];
    ] in


  {O.rotSum = acc.value; O.counter = counter.value}






 