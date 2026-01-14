open! Core
open! Hardcaml

module I = struct
  type 'a t = {
    clk : 'a;
    clr :'a;
    valid : 'a;
    din :'a[@bits 4];
    pos : 'a[@bits 8];
  }[@@deriving hardcaml, sexp_of]
end

module O = struct
  type 'a t = {
    dout :'a[@bits 64];
  }[@@deriving hardcaml, sexp_of]
end

  
let circuit(i : _ I.t) =
  let open Signal in
  let module Case = Hardcaml.With_valid in

  let totalChars = 15 in
  let val1pos = totalChars - 11 in
  let val2pos = totalChars - 10 in
  let val3pos = totalChars - 9 in
  let val4pos = totalChars - 8 in
  let val5pos = totalChars - 7 in
  let val6pos = totalChars - 6 in
  let val7pos = totalChars - 5 in
  let val8pos = totalChars - 4 in
  let val9pos = totalChars - 3 in
  let val10pos = totalChars - 2 in
  let val11pos = totalChars - 1 in
  let val12pos = totalChars - 0 in

  let spec = Reg_spec.create ~clock:i.clk ~clear:i.clr () in
  let finished = (reg spec i.valid) &: ~:(i.valid) in
  (* let finishedReg = reg spec ~enable:i.valid finished in *)
  (*Valid implementation*)
  (*Dislike how this only works for number*)
  let val1 = reg_fb spec ~width:4 ~f:(fun currentVal ->
    (*din greater than current(1). Swap. Else keep(0). Or val2 == 0 so val1 = prev val2(2)*)
    mux2 finished (zero 4) (mux2 (i.valid &: (i.din >: currentVal) &: (i.pos <:. val1pos)) i.din currentVal)
    )in

  let val2 = reg_fb spec ~width:4 ~f:(fun currentVal ->
    let sel1 = ((i.din >: val1)&: (i.pos <:. val1pos)) in
    let sel2 = ((i.din >: currentVal) &: (i.pos <:. val2pos)) in
    let sel = sel1 @: sel2 in
    mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val3 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = ((i.din >: val1)&: (i.pos <:. val1pos)) |: ((i.din >: val2)&:(i.pos <:. val2pos))in
      let sel2 = (i.din >: currentVal)&:(i.pos<:.val3pos) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val4 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = ((i.din >: val1)&: (i.pos <:. val1pos)) |: ((i.din >: val2)&:(i.pos <:. val2pos)) |: ((i.din >: val3)&:(i.pos <:. val3pos))in
      let sel2 = (i.din >: currentVal)&:(i.pos<:.val4pos) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val5 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = ((i.din >: val1)&: (i.pos <:. val1pos)) |: ((i.din >: val2)&:(i.pos <:. val2pos)) |: ((i.din >: val3)&:(i.pos <:. val3pos)) 
    |: ((i.din >: val4)&:(i.pos <:. val4pos))in
      let sel2 = (i.din >: currentVal)&:(i.pos<:.val5pos) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val6 = reg_fb spec ~width:4 ~f:(fun currentVal ->
     let sel1 = ((i.din >: val1)&: (i.pos <:. val1pos)) |: ((i.din >: val2)&:(i.pos <:. val2pos)) |: ((i.din >: val3)&:(i.pos <:. val3pos)) 
    |: ((i.din >: val4)&:(i.pos <:. val4pos)) |: ((i.din >: val5)&:(i.pos <:. val5pos))in
      let sel2 = (i.din >: currentVal)&:(i.pos<:.val6pos) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val7 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = ((i.din >: val1)&: (i.pos <:. val1pos)) |: ((i.din >: val2)&:(i.pos <:. val2pos)) |: ((i.din >: val3)&:(i.pos <:. val3pos)) 
    |: ((i.din >: val4)&:(i.pos <:. val4pos)) |: ((i.din >: val5)&:(i.pos <:. val5pos)) |: ((i.din >: val6)&:(i.pos <:. val6pos)) in
      let sel2 = (i.din >: currentVal)&:(i.pos<:.val7pos) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val8 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = ((i.din >: val1)&: (i.pos <:. val1pos)) |: ((i.din >: val2)&:(i.pos <:. val2pos)) |: ((i.din >: val3)&:(i.pos <:. val3pos)) 
    |: ((i.din >: val4)&:(i.pos <:. val4pos)) |: ((i.din >: val5)&:(i.pos <:. val5pos)) |: ((i.din >: val6)&:(i.pos <:. val6pos))
    |: ((i.din >: val7)&:(i.pos<:.val7pos)) in
      let sel2 = (i.din >: currentVal)&:(i.pos<:.val8pos) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val9 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = ((i.din >: val1)&: (i.pos <:. val1pos)) |: ((i.din >: val2)&:(i.pos <:. val2pos)) |: ((i.din >: val3)&:(i.pos <:. val3pos)) 
    |: ((i.din >: val4)&:(i.pos <:. val4pos)) |: ((i.din >: val5)&:(i.pos <:. val5pos)) |: ((i.din >: val6)&:(i.pos <:. val6pos))
    |: ((i.din >: val7)&:(i.pos<:.val7pos)) |: ((i.din >: val8)&:(i.pos<:.val8pos))in
      let sel2 = (i.din >: currentVal)&:(i.pos<:.val9pos) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val10 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = ((i.din >: val1)&: (i.pos <:. val1pos)) |: ((i.din >: val2)&:(i.pos <:. val2pos)) |: ((i.din >: val3)&:(i.pos <:. val3pos)) 
    |: ((i.din >: val4)&:(i.pos <:. val4pos)) |: ((i.din >: val5)&:(i.pos <:. val5pos)) |: ((i.din >: val6)&:(i.pos <:. val6pos))
    |: ((i.din >: val7)&:(i.pos<:.val7pos)) |: ((i.din >: val8)&:(i.pos<:.val8pos)) |: ((i.din >: val9)&:(i.pos<:.val9pos))in
      let sel2 = (i.din >: currentVal)&:(i.pos<:.val10pos) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val11 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = ((i.din >: val1)&: (i.pos <:. val1pos)) |: ((i.din >: val2)&:(i.pos <:. val2pos)) |: ((i.din >: val3)&:(i.pos <:. val3pos)) 
    |: ((i.din >: val4)&:(i.pos <:. val4pos)) |: ((i.din >: val5)&:(i.pos <:. val5pos)) |: ((i.din >: val6)&:(i.pos <:. val6pos))
    |: ((i.din >: val7)&:(i.pos<:.val7pos)) |: ((i.din >: val8)&:(i.pos<:.val8pos)) |: ((i.din >: val9)&:(i.pos<:.val9pos))
    |: ((i.din >: val10)&:(i.pos<:.val10pos)) in
      let sel2 = (i.din >: currentVal)&:(i.pos<:. val11pos) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val12 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = ((i.din >: val1)&: (i.pos <:. val1pos)) |: ((i.din >: val2)&:(i.pos <:. val2pos)) |: ((i.din >: val3)&:(i.pos <:. val3pos)) 
    |: ((i.din >: val4)&:(i.pos <:. val4pos)) |: ((i.din >: val5)&:(i.pos <:. val5pos)) |: ((i.din >: val6)&:(i.pos <:. val6pos))
    |: ((i.din >: val7)&:(i.pos<:.val7pos)) |: ((i.din >: val8)&:(i.pos<:.val8pos)) |: ((i.din >: val9)&:(i.pos<:.val9pos))
    |: ((i.din >: val10)&:(i.pos<:.val10pos)) |: ((i.din >: val11)&:(i.pos<:.val11pos)) in
      let sel2 = (i.din >: currentVal)&:(i.pos<:.val12pos) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

  

    let finalVal1Reg = reg spec ~enable:finished val1 in
    let finalVal2Reg = reg spec ~enable:finished val2 in
    let finalVal3Reg = reg spec ~enable:finished val3 in
    let finalVal4Reg = reg spec ~enable:finished val4 in
    let finalVal5Reg = reg spec ~enable:finished val5 in
    let finalVal6Reg = reg spec ~enable:finished val6 in
    let finalVal7Reg = reg spec ~enable:finished val7 in
    let finalVal8Reg = reg spec ~enable:finished val8 in
    let finalVal9Reg = reg spec ~enable:finished val9 in
    let finalVal10Reg = reg spec ~enable:finished val10 in
    let finalVal11Reg = reg spec ~enable:finished val11 in
    let finalVal12Reg = reg spec ~enable:finished val12 in


    let finalValList = [finalVal1Reg; 
    finalVal2Reg; 
    finalVal3Reg;
    finalVal4Reg;
    finalVal5Reg;
    finalVal6Reg;
    finalVal7Reg;
    finalVal8Reg;
    finalVal9Reg;
    finalVal10Reg;
    finalVal11Reg;
    finalVal12Reg] in

    let accum = reg_fb spec ~enable:finished ~width:64 ~f:(fun currentVal ->
      let binVal = List.fold_left finalValList ~init:(zero 64) ~f:(fun finalVal cur ->
        let times10 = ((sll ~by:3 finalVal) +: (sll ~by:1 finalVal)) in
        times10 +: (uresize ~width:64 cur)
      ) in
      currentVal +: binVal
      ) in 


    {O.dout = accum}