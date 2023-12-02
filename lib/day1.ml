let is_digit = function '0' .. '9' -> true | _ -> false

let char_of_digit_word = function
  | "one" -> '1'
  | "two" -> '2'
  | "three" -> '3'
  | "four" -> '4'
  | "five" -> '5'
  | "six" -> '6'
  | "seven" -> '7'
  | "eight" -> '8'
  | "nine" -> '9'
  | _ -> raise (Invalid_argument "unknown digit")

let digit_words =
  [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]

let find_digit_word s =
  List.find_opt (fun sx -> String.starts_with ~prefix:sx s) digit_words
  |> Option.map char_of_digit_word

let get_digits line =
  let chars = String.to_seq line |> List.of_seq in

  let rec get_digits' acc rest =
    match rest with
    | hd :: tl -> (
        if is_digit hd then get_digits' (acc @ [ hd ]) tl
        else
          match hd :: tl |> List.to_seq |> String.of_seq |> find_digit_word with
          | Some n -> get_digits' (acc @ [ n ]) tl
          | None -> get_digits' acc tl)
    | _ -> acc
  in

  let digits = get_digits' [] chars in
  let first = List.hd digits in
  let last = List.nth digits (List.length digits - 1) in

  int_of_string (Printf.sprintf "%c%c" first last)

let run () =
  (* let file = "./lib/day_1/sample2.txt" in *)
  let file = "./inputs/day_01/input.txt" in
  let lines = Util.read_lines file in
  let sum_digits = List.fold_left (fun acc l -> acc + get_digits l) 0 in
  sum_digits lines |> string_of_int |> print_endline

(*
 Part 1

let is_digit = function '0' .. '9' -> true | _ -> false

let get_digits line =
  let digits = line |> String.to_seq |> Seq.filter is_digit |> List.of_seq in
  let first = List.hd digits in
  let last = List.nth digits (List.length digits - 1) in
  int_of_string (Printf.sprintf "%c%c" first last)

let main () =
  print_endline "\n";
  let file = "./lib/day_1/input.txt" in
  let lines = Util.read_lines file in
  let sum_digits = List.fold_left (fun acc l -> acc + get_digits l) 0 in
  sum_digits lines |> string_of_int |> print_endline

  *)
