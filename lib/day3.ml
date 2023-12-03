open Base

let sample =
  "467..114..\n\
   ...*......\n\
   ..35..633.\n\
   ......#...\n\
   617*......\n\
   .....+.58.\n\
   ..592.....\n\
   ......755.\n\
   ...$.*....\n\
   .664.598.."

(*

find the length of number
and check the neighbors

  Simplified:
  ..xxx..
  ..x0x..
  ..xxx..

  Multi:
  ..xxxx..
  ..x00x..
  ..xxxx..

  neighbors = N NE E SE S SW W NW

  *)

type grid = row list [@@deriving show]
and row = char list [@@deriving show]

let is_symbol c =
  (not (Char.is_digit c)) && (not (Char.equal c '.')) && not (Char.is_alpha c)

let is_symbol_opt = Option.exists ~f:is_symbol
let is_digit_opt = Option.exists ~f:Char.is_digit

let are_any_neighbors_symbol ~i ~row ~prev ~next =
  let neighbors row i =
    match row with
    | Some row -> [ List.nth row (i - 1); List.nth row i; List.nth row (i + 1) ]
    | None -> []
  in

  let above = neighbors prev i in
  let below = neighbors next i in
  let all_neighbors =
    above @ [ List.nth row (i - 1); List.nth row (i + 1) ] @ below
  in

  all_neighbors |> List.find ~f:is_symbol_opt |> Option.is_some

type maybe_part = { number : int; valid : bool } [@@deriving show]

let rec do_row ~acc ~col ~row ~prev ~next ~sum =
  let cur = List.nth row col in
  match cur with
  | Some cur ->
      let valid =
        if acc.valid then true
        else are_any_neighbors_symbol ~i:col ~row ~prev ~next
      in
      let is_digit = Char.is_digit cur in
      let is_first_digit =
        List.nth row (col - 1) |> is_digit_opt |> not && is_digit
      in
      let is_last_digit =
        List.nth row (col + 1) |> is_digit_opt |> not && is_digit
      in
      let n =
        cur |> String.of_char |> fun x -> try Int.of_string x with _ -> 0
      in
      (* Fmt.pr *)
      (* "'%c' curr: %i, n: %i | digit? %b, first? %b, last? %b nvalid?: %b/%b\n\ *)
         (*   \           %s \n" *)
      (*   cur acc.number n is_digit is_first_digit is_last_digit *)
      (*   (are_any_neighbors_symbol ~i:col ~row ~prev ~next) *)
      (*   valid (show_maybe_part acc); *)
      let number =
        match (is_digit, is_first_digit, is_last_digit) with
        | true, true, true -> n
        | true, true, false -> (acc.number * 10) + n
        | true, false, true -> (acc.number * 10) + n
        | true, false, false -> (acc.number * 10) + n
        | false, false, false -> acc.number
        | _ ->
            failwith
              (Fmt.str "unhandled %b %b %b" is_digit is_first_digit
                 is_last_digit)
      in

      if not is_digit then
        (* Fmt.pr "%s \n" (show_maybe_part acc); *)
        let sum = if acc.valid then sum + acc.number else sum in
        (* reset *)
        do_row
          ~acc:{ valid = false; number = 0 }
          ~col:(col + 1) ~row ~prev ~next ~sum
      else if is_first_digit then
        do_row ~acc:{ valid; number } ~col:(col + 1) ~row ~prev ~next ~sum
      else do_row ~acc:{ valid; number } ~col:(col + 1) ~row ~prev ~next ~sum
  | None -> if acc.valid then sum + acc.number else sum

let run () =
  (* let grid = sample |> String.split_lines |> List.map ~f:String.to_list in *)
  let grid =
    "./inputs/day_03/input.txt" |> Util.read_lines |> List.map ~f:String.to_list
  in
  let _xs =
    List.mapi
      ~f:(fun idx row ->
        let prev = List.nth grid (idx - 1) in
        let next = List.nth grid (idx + 1) in
        do_row ~acc:{ number = 0; valid = false } ~col:0 ~row ~prev ~next ~sum:0)
      grid
  in

  let total = List.reduce_exn ~f:( + ) _xs in

  (* Fmt.pr "?? %s \n\n\n" (show_maybe_part x); *)
  (* Fmt.pr "total valids %i \n" (List.length !all_valids); *)

  (* List.iter ~f:(fun x -> Fmt.pr "-- %i \n" x) !all_valids; *)
  Fmt.pr "total=%i \n " total;

  ()

let%test "is_symbol" = is_symbol '-'
let acc = { number = 0; valid = false }

let%test "edge and below" =
  let row = "..123" |> String.to_list in
  let after = "....#" |> String.to_list in
  let subject = do_row ~acc ~col:0 ~row ~prev:None ~next:(Some after) ~sum:0 in
  Fmt.pr "got %i \n" subject;
  Int.equal subject 123
