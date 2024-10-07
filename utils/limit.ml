(** borrowed from https://discuss.ocaml.org/t/todays-trick-memory-limits-with-gc-alarms/4431 *)
let run_with_memory_limit limit f =
  let limit_memory () =
    let mem = Gc.(quick_stat ()).heap_words in
    if mem > limit / (Sys.word_size / 8) then raise Out_of_memory
  in
  let alarm = Gc.create_alarm limit_memory in
  Fun.protect f ~finally:(fun () -> Gc.delete_alarm alarm ; Gc.compact ())
