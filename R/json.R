# Sentinel check: TRUE when query_json() returned NA_complex_ (path not found).
json_missing <- function(x) identical(x, NA_complex_)

# Query a JSON pointer from a file.
#
# Uses NA_complex_ as a sentinel value (JSON has no complex type) to detect
# missing paths. When required = TRUE (default) and the path is absent, walks
# backwards through the pointer segments to pinpoint the first missing node and
# throws an informative error. When required = FALSE, returns NA_complex_ silently.
query_json <- function(
  json_path,
  query,
  required = TRUE,
  .call = caller_call()
) {
  result <- RcppSimdJson::fload(
    json_path,
    query = query,
    query_error_ok = TRUE,
    on_query_error = NA_complex_
  )
  if (!json_missing(result)) {
    return(result)
  }
  if (!required) {
    return(NA_complex_)
  }

  # Walk backwards through segments to find the last existing node
  segments <- strsplit(sub("^\\/", "", query), "/", fixed = TRUE)[[1]]
  last_valid <- "(root)"
  missing_idx <- 1L
  for (i in rev(seq_len(length(segments) - 1L))) {
    path <- paste0("/", paste(segments[seq_len(i)], collapse = "/"))
    parent <- RcppSimdJson::fload(
      json_path,
      query = path,
      query_error_ok = TRUE,
      on_query_error = NA_complex_
    )
    if (!json_missing(parent)) {
      last_valid <- paste0(path, "/")
      missing_idx <- i + 1L
      break
    }
  }
  missing_path <- paste(segments[missing_idx:length(segments)], collapse = "/")
  cli_abort(
    c(
      "missing node in JSON path  {col_green(last_valid)}{col_red(missing_path)}"
    ),
    call = .call
  )
}
