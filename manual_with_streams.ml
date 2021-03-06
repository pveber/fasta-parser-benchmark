open Core.Std
open CFStream

type item = {
  comment : string ;
  seq : string
}

let chopl s = String.(sub s ~pos:1 ~len:(length s - 1))

let is_comment x = String.is_prefix x ~prefix:">"

let read_sequence lines =
  let open Or_error in
  match Stream.next lines with
  | None -> error_string "Fasta.read: incorrect syntax"
  | Some l ->
    let rec loop lines accu =
      match Stream.peek lines with
      | None -> accu, lines
      | Some l ->
        if is_comment l then accu, lines
        else (
          Stream.junk lines ;
          loop lines (l :: accu)
        )
    in
    if is_comment l then error_string "Fasta.read: incorrect syntax"
    else (
      let seqs, lines = loop lines [l] in
      return (String.concat (List.rev seqs), lines)
    )

let read_item lines =
  let open Or_error in
  match Stream.next lines with
  | None -> None
  | Some l ->
    if String.is_prefix l ~prefix:">" then
      let comment = chopl l in
      match read_sequence lines with
      | Result.Ok (seq, lines) ->
        Some (return { comment ; seq }, lines)
      | Result.Error e -> Some (Result.Error e, Stream.empty ())
    else
      Some (error_string "Fasta.read_item: incorrect syntax", Stream.empty ())

let of_channel ic =
  Biocaml_lines.of_channel ic
  |> Stream.map ~f:(fun x -> (x : Biocaml_line.t :> string))
  |> Stream.unfold ~f:read_item

let total_length fn =
  In_channel.with_file fn ~f:(fun ic ->
      Stream.fold (of_channel ic) ~init:0 ~f:(fun accu it -> accu + String.length (ok_exn it).seq)
    )
