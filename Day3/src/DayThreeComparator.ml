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
    dout :'a[@bits 12];
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

    let val3 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = (i.din >: val1) |: (i.din >: val2) in
      let sel2 = (i.din >: currentVal) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val4 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = (i.din >: val1) |: (i.din >: val2) |: (i.din >: val3) in
      let sel2 = (i.din >: currentVal) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val5 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = (i.din >: val1) |: (i.din >: val2) |: (i.din >: val3) |: (i.din >: val4)in
      let sel2 = (i.din >: currentVal) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val6 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = (i.din >: val1) |: (i.din >: val2) |: (i.din >: val3) |: (i.din >: val4) |: (i.din >: val5)in
      let sel2 = (i.din >: currentVal) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val7 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = (i.din >: val1) |: (i.din >: val2) |: (i.din >: val3) |: (i.din >: val4) |: (i.din >: val5) |: (i.din >: val6) in
      let sel2 = (i.din >: currentVal) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val8 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = (i.din >: val1) |: (i.din >: val2) |: (i.din >: val3) 
      |: (i.din >: val4) |: (i.din >: val5) |: (i.din >: val6) |: (i.din >: val7)in
      let sel2 = (i.din >: currentVal) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val9 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = (i.din >: val1) |: (i.din >: val2) |: (i.din >: val3)
    |: (i.din >: val4) |: (i.din >: val5) |: (i.din >: val6) |: (i.din >: val7) |: (i.din >: val8) in
      let sel2 = (i.din >: currentVal) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val10 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = (i.din >: val1) |: (i.din >: val2) |: (i.din >: val3)
    |: (i.din >: val4) |: (i.din >: val5) |: (i.din >: val6) |: (i.din >: val7) |: (i.din >: val8) 
    |: (i.din >: val9) in
      let sel2 = (i.din >: currentVal) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val11 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = (i.din >: val1) |: (i.din >: val2) |: (i.din >: val3)
    |: (i.din >: val4) |: (i.din >: val5) |: (i.din >: val6) |: (i.din >: val7) |: (i.din >: val8) 
    |: (i.din >: val9) |: (i.din >: val10) in
      let sel2 = (i.din >: currentVal) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let val12 = reg_fb spec ~width:4 ~f:(fun currentVal ->
      let sel1 = (i.din >: val1) |: (i.din >: val2) |: (i.din >: val3)
    |: (i.din >: val4) |: (i.din >: val5) |: (i.din >: val6) |: (i.din >: val7) |: (i.din >: val8) 
    |: (i.din >: val9) |: (i.din >: val10) |: (i.din >: val11) in
      let sel2 = (i.din >: currentVal) in
      let sel = sel1 @: sel2 in
       mux2 finished (zero 4) (mux sel [currentVal;i.din; of_int_trunc ~width:4 0; of_int_trunc ~width:4 0])
    ) in

    let finalValCur = (val1 @: val2 @: val3 @: val4 @: val5 @: val6 @: val7 @: val8 @: val9 @: val10 @: val11 @: val12) in

    (*Pipelines*)
    let finalDel1 = pipeline spec ~n:1 ~enable:i.valid finalValCur in
    let finalDel2 = pipeline spec ~n:2 ~enable:i.valid finalValCur in
    let finalDel3 = pipeline spec ~n:3 ~enable:i.valid finalValCur in
    let finalDel4 = pipeline spec ~n:4 ~enable:i.valid finalValCur in
    let finalDel5 = pipeline spec ~n:5 ~enable:i.valid finalValCur in
    let finalDel6 = pipeline spec ~n:6 ~enable:i.valid finalValCur in
    let finalDel7 = pipeline spec ~n:7 ~enable:i.valid finalValCur in
    let finalDel8 = pipeline spec ~n:8 ~enable:i.valid finalValCur in
    let finalDel9 = pipeline spec ~n:9 ~enable:i.valid finalValCur in
    let finalDel10 = pipeline spec ~n:10 ~enable:i.valid finalValCur in
    let finalDel11 = pipeline spec ~n:11 ~enable:i.valid finalValCur in
    let finalDel12 = pipeline spec ~n:12 ~enable:i.valid finalValCur in

    let finalVal = 


    let finalReg = reg spec ~enable:finished finalVal in

    {O.dout = finalReg}
