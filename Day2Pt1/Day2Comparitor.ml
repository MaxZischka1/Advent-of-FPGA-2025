(*First we should subtract the upper and lower bound to provide 1 piece of data. The 
number of digits changed. So now if we have a length input, data input and # of digits 
  we want to be able to think of some way to concatenate with. 

For the 1 digit check concatenate all of the first digit. For the second digit*)

open! Core
open! Hardcaml

module I = struct
  type 'a t = {
    uBound :'a[@bits 40]; (*Hopefully can find a better approach that doesn't use both sizes*)
    lBound : 'a[@bits 40];
    uSize : 'a[@bits 4];
    lSize : 'a[@bits 4];
    valid : 'a;
  }
end

module O = struct
  type 'a t = {
    idSum : 'a[@bits 64]; (*Insanely large*)
  }
end