open Core.Std

let time f x =
  let start = Time.(now () |> to_float) in
  let y = f x in
  let stop = Time.(now () |> to_float) in
  (y, stop -. start)



let input_file = Sys.argv.(1)

let functions = [
  "manual-direct", Manual_direct.total_length ;
  "manual-direct-lwt", Manual_direct_lwt.total_length ;
  "manual-direct-exn", Manual_direct_exn.total_length ;
  "manual-with-streams", Manual_with_streams.total_length ;
  "manual-with-lazy-lists", Manual_withlazy_lists.total_length ;
(*   "lex-with-streams", Lex_with_streams.total_length ; *)
  "biocaml-with-streams", Biocaml_with_streams.total_length ;
]

let bench to_string fs =
  List.iter fs ~f:(fun (label, f) ->
      let (y, t) = time f input_file in
      printf "% 32s | % 16.2f | % 16s\n%!" label t (to_string y)
    )

let () = bench string_of_int functions
