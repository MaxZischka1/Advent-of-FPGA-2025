open! Core
let parse val_str =
  val_str
  |> String.split_lines
  |> List.map ~f:(fun line ->
      let dir_char = line.[0] in

      let num_str = String.sub line ~pos:1 ~len:(String.length line - 1) in

      let num_val = Int.of_string num_str in
      (dir_char, num_val)
  );