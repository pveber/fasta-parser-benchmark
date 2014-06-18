open Core.Std
open CFStream
module LL = BatLazyList

type item = {
  comment : string ;
  seq : string
}

let chopl s = String.(sub s ~pos:1 ~len:(length s - 1))

let is_comment x = String.is_prefix x ~prefix:">"

let read_sequence lines =
  let open Or_error in
  match Lazy.force lines with
  | LL.Nil -> error_string "Fasta.read: incorrect syntax"
  | LL.Cons (l, tail) ->
    let rec loop lines accu =
      match Lazy.force lines with
      | LL.Nil -> accu, LL.nil
      | LL.Cons (l, tail) ->
        if is_comment l then accu, lines
        else loop tail (l :: accu)
    in
    if is_comment l then error_string "Fasta.read: incorrect syntax"
    else (
      let seqs, lines = loop tail [l] in
      return (String.concat (List.rev seqs), lines)
    )

let read_item lines =
  let open Or_error in
  match Lazy.force lines with
  | LL.Nil -> None
  | LL.Cons (l,tail) ->
    if String.is_prefix l ~prefix:">" then
      let comment = chopl l in
      match read_sequence tail with
      | Result.Ok (seq, lines) ->
        Some (return { comment ; seq }, lines)
      | Result.Error e -> Some (Result.Error e, LL.nil)
    else
      Some (error_string "Fasta.read_item: incorrect syntax", LL.nil)

let of_channel ic =
  Biocaml_lines.of_channel ic
  |> LL.of_stream
  |> LL.map (fun x -> (x : Biocaml_line.t :> string))
  |> (fun x -> LL.unfold x read_item)

let total_length fn =
  In_channel.with_file fn ~f:(fun ic ->
      LL.fold_left (fun accu it -> accu + String.length (ok_exn it).seq) 0 (of_channel ic)
    )
