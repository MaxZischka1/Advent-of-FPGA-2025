open! Core
open! Hardcaml
open! DayOne
open! CommonLib

module I = struct
  type 'a t = {
    clr : 'a;
    clk : 'a;
    rx : 'a;
  } [@@deriving hardcaml, sexp_of]
end

module O = struct
  type 'a t = {
    dinOut : 'a[@bits 8];
    curSum : 'a[@bits 8];
    count : 'a[@bits 16];
  } [@@deriving hardcaml, sexp_of]
end

let circuit(i : _ I.t) =

  let(data,validu) = Uart_rx.initialize ~clock:i.clk ~rx:i.rx in

  let counter_out = Day1Adder.circuit {
    Day1Adder.I.clk = i.clk;
    Day1Adder.I.clr = i.clr;
    Day1Adder.I.din = data;
    Day1Adder.I.valid = validu;
  } in

  {O.count = counter_out.counter; O.curSum = counter_out.rotSum; O.dinOut = counter_out.dinOut}
