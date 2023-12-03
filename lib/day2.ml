open Base
open Stdio

let rounds =
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
   Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
   Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
   Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
   Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" |> String.split_lines

type color = Blue | Green | Red [@@deriving show]
type hand = { value : int; color : color } [@@deriving show]
type set = hand list [@@deriving show]
type game = { num : int; sets : set list } [@@deriving show]

let color_of_string s =
  match s with
  | "blue" -> Some Blue
  | "green" -> Some Green
  | "red" -> Some Red
  | _ -> None

type token = Game | Int of int | Colon | Comma | Color of color | Semicolon
[@@deriving show]

let lookup_identifier s =
  match color_of_string s with
  | Some color -> Color color
  | None ->
      if String.equal s "Game" then Game
      else raise (Invalid_argument (Printf.sprintf "unknown identifier %s" s))

let read_identifier chars =
  let rec aux acc rest =
    match rest with
    | h :: tl when Char.is_alpha h -> aux (acc ^ Char.to_string h) tl
    | _ -> (acc, rest)
  in
  let ident, rest = aux "" chars in
  (* print_endline ("found ident " ^ ident); *)
  (lookup_identifier ident, rest)

let read_number chars =
  let rec aux acc rest =
    match rest with
    | h :: tl when Char.is_digit h -> aux (acc ^ Char.to_string h) tl
    | _ -> (acc, rest)
  in
  let num, rest = aux "" chars in
  (Int.of_string num, rest)

let tokenize chars =
  let rec tokenize' acc rest =
    match rest with
    | h :: tl -> (
        match h with
        | h when Char.is_digit h ->
            let num, rest = read_number (h :: tl) in
            tokenize' (acc @ [ Int num ]) rest
        | h when Char.equal h ':' -> tokenize' (acc @ [ Colon ]) tl
        | h when Char.equal h ',' -> tokenize' (acc @ [ Comma ]) tl
        | h when Char.equal h ';' -> tokenize' (acc @ [ Semicolon ]) tl
        | h when Char.is_alpha h ->
            let ident, rest = read_identifier (h :: tl) in
            tokenize' (acc @ [ ident ]) rest
        | h when Char.is_whitespace h -> tokenize' acc tl
        | _ -> raise (Invalid_argument (Printf.sprintf "unknown token %c" h)))
    | _ -> acc
  in

  tokenize' [] chars

let parse_statement tokens =
  let parse_game_num tokens =
    match List.hd tokens with
    | Some (Int i) -> (i, List.tl_exn tokens)
    | _ -> raise (Invalid_argument "expected next token to be an int")
  in

  if not (phys_equal (List.hd_exn tokens) Game) then
    raise (Invalid_argument "Must start with game token")
  else ();

  (* we know it starts with game, so do this before recursing *)
  let num, tokens = parse_game_num (List.tl_exn tokens) in

  let initial = { num; sets = [] } in

  let parse_set tokens : hand list * token list =
    let rec parse_set' acc tokens =
      match tokens with
      | Int value :: Color color :: tl ->
          parse_set' (acc @ [ { value; color } ]) tl
      | Comma :: tl -> parse_set' acc tl
      | tl -> (acc, tl)
    in
    parse_set' [] tokens
  in

  let rec aux acc tokens =
    match tokens with
    | h :: tl -> (
        match h with
        | Colon | Semicolon ->
            let set, rest = parse_set tl in
            aux { acc with sets = acc.sets @ [ set ] } rest
        | _ ->
            (* List.iter ~f:(fun x -> print_endline (show_token x)) tokens; *)
            raise
              (Invalid_argument
                 (Printf.sprintf " failed to match token %s" (show_token h))))
    | _ -> acc
  in

  aux initial tokens

let max_red = 12
let max_green = 13
let max_blue = 14

let color_over_limit color value =
  match color with
  | Red -> value > max_red
  | Green -> value > max_green
  | Blue -> value > max_blue

let is_valid_game (game : game) : bool =
  let rec is_invalid_game sets =
    match sets with
    | h :: tl ->
        if color_over_limit h.color h.value then true else is_invalid_game tl
    | [] -> false
  in

  List.find ~f:is_invalid_game game.sets |> Option.is_none

let run () =
  (* let chars = rounds |> String.to_list in *)
  let parse_game round =
    (* print_endline "\n"; *)
    (* print_endline round; *)
    let g =
      round |> String.to_list |> tokenize
      (* |> (fun x -> *)
      (*      List.iter ~f:(fun y -> print_endline (show_token y)) x; *)
      (*      x) *)
      |> parse_statement
    in
    (* print_endline (show_game g); *)
    g
  in

  let file = "./inputs/day_02/input.txt" in
  let rounds = Util.read_lines file in
  let result =
    rounds |> List.map ~f:parse_game
    |> List.filter ~f:is_valid_game
    |> List.fold_left ~init:0 ~f:(fun acc g -> acc + g.num)
  in
  print_endline "\n";
  print_endline (Int.to_string result);
  ()
