(*

the lexer produced by ocamllex fails on large input with a stack overflow

*)
open Core.Std
open CFStream

type item = {
  header : string ;
  sequence : string
}

let token_stream_of_channel ic =
  let lexbuf = Lexing.from_channel ic in
  let f _ =
    try Some (Fasta_lexer.token lexbuf)
    with Failure _ -> None
  in
  Stream.from f

let read_sequence tokens = match Stream.peek tokens with
  | Some (Fasta_lexer.SEQUENCE s) ->
    let rec loop tokens accu = match Stream.peek tokens with
      | Some (Fasta_lexer.SEQUENCE s) ->
        loop (Stream.skip tokens 1) (accu ^ s)
      | _ -> accu, tokens
    in
    let r, tokens = loop (Stream.skip tokens 1) "" in
    `Ok r, tokens
  | _ -> `Error "syntax error", Stream.empty ()

let read_item tokens = match Stream.peek tokens with
  | None -> None
  | Some (Fasta_lexer.HEADER header) -> (
      match read_sequence (Stream.skip tokens 1) with
      | `Ok sequence, tokens ->
        Some (`Ok { header ; sequence }, tokens)
      | `Error e, _ -> Some (`Error e, Stream.empty ())
    )
  | _ -> Some (`Error "syntax error", Stream.empty ())

let of_channel ic =
  token_stream_of_channel ic
  |> Stream.unfold ~f:read_item

let ok_exn = function
  | `Ok x -> x
  | `Error e -> failwith e

let total_length fn =
  In_channel.with_file fn ~f:(fun ic ->
      Stream.fold (of_channel ic) ~init:0 ~f:(fun accu it -> accu + String.length (ok_exn it).sequence)
    )
