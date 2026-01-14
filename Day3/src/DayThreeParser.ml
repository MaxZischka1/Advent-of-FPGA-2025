open! Core
let lineParser2 vals = 
  vals
  |> String.split_lines
  |> List.map ~f:(fun line ->
    line
    |> String.to_list
    |> List.mapi ~f:(fun i chr -> (Char.to_int chr - Char.to_int '0'),i)
    )