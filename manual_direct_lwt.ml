open Core.Std
open CFStream
open Lwt

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

let read_line ic =
  Lwt_io.read_line_opt ic >>= fun lo ->
  return (Option.map lo ~f:classify_line)

let read_sequence ic =
  let open Result in
  match_lwt read_line ic with
  | Some (Sequence s) ->
    let rec loop accu = match_lwt read_line ic with
      | None -> Lwt.return (accu, None)
      | Some (Sequence s) -> loop (s :: accu)
      | Some l -> Lwt.return (accu, Some l)
in
lwt seqs, cursor = loop [s] in
    Lwt.return (return (String.concat (List.rev seqs), cursor))
  | _ ->
    Lwt.return (fail "missing_sequence_after_header")


let read_item l ic =
  let open Result in
  match l with
  | Header header -> (
    match_lwt read_sequence ic with
    | Ok (sequence, cursor) ->
      Lwt.return (return { header ; sequence }, cursor)
    | Error e -> Lwt.return (fail e, None)
    )
  | _ -> Lwt.return (fail "expected_header", None)

let of_channel ic =
  lwt first = read_line ic in
let v = ref first in
  let f () = match !v with None -> return None | Some l -> lwt y, next = read_item l ic in (v := next ; return (Some y)) in
  return (Lwt_stream.from f)

let ok_exn = function
  | Result.Ok x -> x
  | Error _ -> failwith "ok_exn"

let total_length fn =
  Lwt_io.with_file ~buffer_size:65536 ~mode:Lwt_io.input fn (fun ic ->
      of_channel ic >>= fun xs -> 
      Lwt_stream.fold (fun it accu -> accu + String.length (Result.ok_or_failwith it).sequence) xs 0
    )
  |> Lwt_main.run
