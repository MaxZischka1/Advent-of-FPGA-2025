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
    dout :'a[@bits 32];
  }[@@deriving hardcaml, sexp_of]
end

  
let circuit(i : _ I.t) =
  let open Signal in
   let module Case = Hardcaml.With_valid in

  let spec = Reg_spec.create ~clock:i.clk ~clear:i.clr () in


    let finished = (reg spec i.valid) &: ~:(i.valid) in

    (* let finishedReg = reg spec ~enable:i.valid finished in *)


  (*Valid implementation*)

  let val1 = reg_fb spec ~width:4 ~f:(fun currentVal ->
    (*din greater than current(1). Swap. Else keep(0). Or val2 == 0 so val1 = prev val2(2)*)
    mux2 finished (zero 4) (mux2 (i.valid &: (i.din >: currentVal)) i.din currentVal)
    )in

  let val2 = reg_fb spec ~width:4 ~f:(fun currentVal ->
    let sel1 = i.din >: val1 in
    let sel2 = i.din >: currentVal in
    let sel = sel1 @: sel2 in
    mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val3 = reg spec ~enable:(i.valid) val1 in 

    let finalVal = mux2 (val2 ==:. 0) (val3 @: val1) (val1 @: val2) in

    let finalReg = reg spec ~enable:finished finalVal in


    let accum = reg_fb spec ~enable:finished ~width:32 ~f:(fun currentVal ->
      let upperNib = sel_top ~width:4 finalReg in
      let upper8 = uresize ~width:8 upperNib in
      let lowerNib = sel_bottom ~width:4 finalReg in
      let lower8 = uresize ~width:8 lowerNib in
      let tensVal = (sll ~by:3 upper8) +: (sll ~by:1 upper8) in
      let newVal = tensVal +: lower8 in
      let newVal32 = uresize ~width:32 newVal in
      (currentVal +: newVal32)
      ) in

      {O.dout = accum}
