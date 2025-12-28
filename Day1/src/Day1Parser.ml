open! Core
let parse val_str =
  val_str
  |> String.split_lines
  |> List.map ~f:(fun line ->
      let dir_char = line.[0] in
      let num_str = String.sub line ~pos:1 ~len:(String.length line - 1) in
      let num_val_org = Int.of_string num_str in
      let num_val = 
        if num_val_org >= 100 then 
          num_val_org mod 100 
        else 
          num_val_org 
          in
        (dir_char, num_val)
  );