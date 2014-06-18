open Core.Std
open CFStream

type item = {
  header : string ;
  sequence : string
}

let chopl s = String.(sub s ~pos:1 ~len:(length s - 1))

type line =
  | Header of string
  | Sequence of string
  | Empty

let classify_line = function
  | "" -> Empty
  | s when s.[0] = '>' -> Header (chopl s)
  | s -> Sequence s

let read_line ic = Option.map (In_channel.input_line ic) ~f:classify_line

let read_sequence ic =
  let open Result in
  match read_line ic with
  | Some (Sequence s) ->
    let rec loop accu = match read_line ic with
      | None -> accu, None
      | Some (Sequence s) -> loop (s :: accu)
      | Some l -> accu, Some l
    in
    let seqs, cursor = loop [s] in
    return (String.concat (List.rev seqs), cursor)
  | _ ->
    fail "missing_sequence_after_header"


let read_item l ic =
  let open Result in
  match l with
  | Header header -> (
    match read_sequence ic with
    | Ok (sequence, cursor) ->
      return { header ; sequence }, cursor
    | Error e -> fail e, None
    )
  | _ -> fail "expected_header", None

let of_channel ic =
  let f cursor = Option.map cursor ~f:(fun l -> read_item l ic) in
  Stream.unfold (read_line ic) f

let ok_exn = function
  | Result.Ok x -> x
  | Error _ -> failwith "ok_exn"

let total_length fn =
  In_channel.with_file fn ~f:(fun ic ->
      Stream.fold (of_channel ic) ~init:0 ~f:(fun accu it -> accu + String.length (Result.ok_or_failwith it).sequence)
    )
