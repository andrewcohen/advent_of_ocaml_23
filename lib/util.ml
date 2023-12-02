let read_lines file =
  let ic = open_in file in
  let try_read () = try Some (input_line ic) with End_of_file -> None in
  let rec read_lines' () =
    match try_read () with
    | Some line -> line :: read_lines' ()
    | None ->
        close_in ic;
        []
  in
  read_lines' ()
