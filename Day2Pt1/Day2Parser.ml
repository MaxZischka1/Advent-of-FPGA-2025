(*Find the size of the string
send that value over along with the BCD values of the digits. 
Sol: Create parser which just passes the bounds or maybe passes every single value
Then we can create a parrallel comparator based on the soze and pattern match
So we will create specific structures for pattern matching. Based on size.
The size variable should also be passed so we minimize the pattern matches we have to do.
If the size is 5 we only have to (AAAA) checks but if 6 we have to (AAAA), (ABAB) and (ABCABC).
The can all be optimized by using for statements in parrallel. The generate statement in verilog.
Research how to do in hardcaml. Special cases are change in variable size. For example the variables
may go from 5 to 6 and this means we no longer check just matches of 1 but also 2 and 3.
We could because were doing it in parrallel just do checks from 1 to 5 of all variables
*)

open! Core

let parse2 str_range = 
  str_range
  |> String.split_on_chars ~on:[',']
  |> List.map ~f:(fun line ->
    let splitPos = String.index_exn line '-' in
    let lowerBound = String.sub ~pos:0 ~len:(splitPos) line in
    let lenUpper = ((String.length line)-1) - (splitPos) in
    let upperBound = String.sub ~pos:(splitPos+1) ~len:lenUpper line in
    let lowerBoundInt = Int.of_string lowerBound in
    let upperBoundInt = Int.of_string upperBound in
    (lowerBoundInt,upperBoundInt, (String.length lowerBound), (String.length upperBound))
    )