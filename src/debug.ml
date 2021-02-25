open Printf

let time (name: string) func =
    let t_start = Mtime_clock.elapsed () in
    let res = func () in
    let t_end = Mtime_clock.elapsed () in
    let t_diff = Mtime.Span.abs_diff t_start t_end in
    let ms_passed = int_of_float @@ floor @@ Mtime.Span.to_ms t_diff in
    printf "%s completed in %dms\n" name ms_passed;
    res
