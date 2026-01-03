open! Core
let rec bcdConv valu =
  if valu = 0 then
    0
  else
    let lastVal = valu mod 10 in

    (bcdConv (valu/10) lsl 4) lor lastVal

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
    let upperBoundBcd = bcdConv upperBoundInt in
    let lowerBoundBcd = bcdConv lowerBoundInt in
    (lowerBoundBcd,upperBoundBcd, (String.length lowerBound), (String.length upperBound))
    )


