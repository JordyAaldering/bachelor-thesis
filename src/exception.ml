


exception InferenceFailure of string

let infer_err msg =
    raise @@ InferenceFailure msg


exception RewriteFailure of string

let rewrite_err msg =
    raise @@ RewriteFailure msg
