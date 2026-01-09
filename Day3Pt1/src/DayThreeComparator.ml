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
    dout :'a[@bits 8];
  }[@@deriving hardcaml, sexp_of]
end

  
let circuit(i : _ I.t) =
  let open Signal in
   let module Case = Hardcaml.With_valid in

  let spec = Reg_spec.create ~clock:i.clk ~clear:i.clr () in

  (*Valid implementation*)

  let val1 = reg_fb spec ~width:4 ~f:(fun currentVal ->
    (*din greater than current(1). Swap. Else keep(0). Or val2 == 0 so val1 = prev val2(2)*)
    mux2 (i.valid &: (i.din >: currentVal)) i.din currentVal
    )in

  let val2 = reg_fb spec ~width:4 ~f:(fun currentVal ->
    let sel1 = i.din >: val1 in
    let sel2 = i.din >: currentVal in
    let sel = sel1 @: sel2 in
    mux sel [currentVal; i.din; val1;]
    ) in

    { O.dout = concat_lsb [val2; val1] }




