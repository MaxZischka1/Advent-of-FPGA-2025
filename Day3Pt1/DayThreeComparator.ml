open! Core
open! Hardcaml

module I = struct
  type 'a t = {
    clk : 'a;
    clr :'a;
    valid : 'a;
    din :'a[@bits 8];
  }[@@deriving hardcaml, sexp_of]
end

module O = struct
  type 'a t = {
    dout :'a[@bits 16];
  }[@@deriving hardcaml, sexp_of]
end

let doubleDabble ~data = 


