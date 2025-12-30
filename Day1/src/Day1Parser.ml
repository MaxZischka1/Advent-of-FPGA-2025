open! Core
let parse val_str =
  val_str
  |> String.split_lines
  |> List.map ~f:(fun line ->
      let dir_char = line.[0] in
      let dir_charInt = Char.to_int dir_char in (*Help with testbench*)
      let num_str = String.sub line ~pos:1 ~len:(String.length line - 1) in
      let num_val_org = Int.of_string num_str in
      let hundVal = num_val_org / 100 in
      let num_val = 
        if num_val_org >= 100 then 
          num_val_org mod 100 
        else 
          num_val_org 
          in
        (dir_charInt, num_val, hundVal)
  );