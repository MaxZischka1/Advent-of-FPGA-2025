open! Core

let lineParser2 vals = 
  let rec makePair = function
  | x :: y :: t -> (x*10) + y :: makePair t
  | _ -> [] (*Will never have only 1 char remaining*)
  in
  vals
  |> String.split_lines
  |> List.map ~f:(fun line ->
    line
    |> String.to_list
    |> List.map ~f:(fun chr -> Char.to_int chr - Char.to_int '0')
    |> makePair
    )