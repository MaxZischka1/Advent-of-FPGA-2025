open! Core
open! Hardcaml

module I = struct
  (*I/O Struct*)
  type 'a t = {
    clk : 'a;
    clr : 'a;
    char : 'a[@bits 8];
    din : 'a[@bits 8]; 
    hundVal : 'a[@bits 8];
    valid : 'a;
  }[@@deriving hardcaml, sexp_of]
end

module O = struct
  type 'a t = {
  counter : 'a[@bits 8];
  }[@@deriving hardcaml, sexp_of]
end

let circuit(i : _ I.t) = 
  let open Signal in

  let spec = Reg_spec.create ~clock:i.clk ~clear:i.clr () in 

  (*Combinational logic using wire loops*)
  let acc = reg_fb spec ~enable:i.valid ~width:8 ~f:(fun currentVal -> (*Mux2 logic backwards?*)
    let dir = (i.char ==:. 76) in

    let nextVal = mux2 dir (currentVal -: i.din) (currentVal +: i.din) in

    let ovfFlag = (nextVal >=:. 100) in

    let correction = mux2 dir (nextVal +:. 100) (nextVal -:. 100) in

    mux2 ovfFlag correction nextVal 
  ) in

  {O.counter = acc}
  




  


 