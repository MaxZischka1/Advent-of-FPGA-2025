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
  let accW = Signal.wire 8 in

  let dir = (i.char ==:. 76) in

  let nextVal = mux2 dir (accW -: i.din) (accW +: i.din) in

  let ovfFlag = (nextVal >=:. 100) in

  let correction = mux2 dir (accW +:. 100) (accW -:. 100) in

  let nextSum = mux2 ovfFlag correction nextVal in
  let sumReg = reg spec ~enable:i.valid nextSum in

  let _ = accW <=: sumReg in

  {O.counter = accW}
  




  


 