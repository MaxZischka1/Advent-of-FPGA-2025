open! Core
open! Hardcaml

module I = struct
  type 'a t = {
    clk : 'a;
    clr :'a;
    valid : 'a;
    din :'a[@bits 4];
  }[@@deriving hardcaml, sexp_of]
end

module O = struct
  type 'a t = {
    dout :'a[@bits 10];
  }[@@deriving hardcaml, sexp_of]
end

  
let circuit(i : _ I.t) =
  let open Signal in

  let spec = Reg_spec.create ~clock:i.clk ~clear:i.clr () in

  let bits = List.init 10 ~f:(fun j ->
    let flag = (i.din ==: of_int_trunc ~width:4 j) &: i.valid in

    reg_fb ~width:1 spec ~f:(fun curr ->
    curr |: flag
    )
  ) in

  {O.dout = concat_lsb bits}



