let debug_print = ref true

let print (msg: string) =
    if !debug_print then Printf.printf "%s\n" msg else ()

let time (name: string) func =
    let t_start = Mtime_clock.elapsed () in
    let res = func () in
    let t_end = Mtime_clock.elapsed () in
    let t_diff = Mtime.Span.abs_diff t_start t_end in
    let ms_passed = int_of_float @@ floor @@ Mtime.Span.to_ms t_diff in
    print @@ Printf.sprintf "%s completed in %dms\n" name ms_passed;
    res
