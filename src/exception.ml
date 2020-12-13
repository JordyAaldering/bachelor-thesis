exception ValueFailure of string

let value_err msg =
    raise @@ ValueFailure msg


exception ParseFailure of string

let parse_err msg =
    raise @@ ParseFailure msg


exception EvalFailure of string

let eval_err msg =
    raise @@ EvalFailure msg


exception InferenceFailure of string

let infer_err msg =
    raise @@ InferenceFailure msg


exception RewriteFailure of string

let rewrite_err msg =
    raise @@ RewriteFailure msg
