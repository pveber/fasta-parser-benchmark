open Core.Std
open CFStream
open Biocaml

let total_length fn =
  In_channel.with_file fn ~f:(fun ic ->
      Stream.fold (Fasta.in_channel_to_char_seq_item_stream_exn ic) ~init:0 ~f:(fun accu it -> accu + String.length it.Fasta.sequence)
    )
