# Sentinel check: TRUE when query_json() returned NA_complex_ (path not found).
json_missing <- function(x) identical(x, NA_complex_)

# Query one or more JSON pointers from a file, returning the first that exists.
#
# query may be a character vector; each entry is tried in order. Uses NA_complex_
# as a sentinel (JSON has no complex type). When required = TRUE (default) and no
# path is found, walks backwards to pinpoint the first missing node in each tried
# path and throws an informative error. When required = FALSE, returns NA_complex_.
query_json <- function(
  json_path,
  query,
  required = TRUE,
  list_as_tibble = FALSE,
  .call = caller_call()
) {
  for (q in query) {
    result <- RcppSimdJson::fload(
      json_path,
      query = q,
      query_error_ok = TRUE,
      on_query_error = NA_complex_
    )
    if (!json_missing(result)) {
      # check if we always want lists as tibble
      if (list_as_tibble && is.data.frame(result)) {
        # already a data frame -> just make it a tibble
        return(tibble::as_tibble(result))
      } else if (list_as_tibble && is.list(result)) {
        # still a list, list elements needs an extra nesting for as_tibble_row() to work
        list_fields <- purrr::map_lgl(result, is.list)
        result[list_fields] <- result[list_fields] |> purrr::map(list)
        return(tibble::as_tibble_row(result))
      } else {
        # just return plain result
        return(result)
      }
    }
  }
  if (!required) {
    return(NA_complex_)
  }

  diags <- query |> purrr::map_chr(diagnose_json_path, json_path = json_path)
  cli_abort(
    c(
      "missing node in JSON {qty(diags)}path{?s}",
      setNames(diags, rep("i", length(diags)))
    ),
    call = .call
  )
}

# Walk backwards through a JSON pointer to produce a colored diagnosis string.
diagnose_json_path <- function(json_path, query) {
  segments <- strsplit(sub("^\\/", "", query), "/", fixed = TRUE)[[1]]
  last_valid <- "(root)/"
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
  paste0(col_green(last_valid), col_red(missing_path))
}

# Find the integer index of a named CBlockData object
# Aborts if the label is not found.
find_json_block_idx_by_label <- function(
  json_path,
  block_query,
  label,
  max_idx = 100L
) {
  i <- 0L
  while (i < max_idx) {
    label_i <- query_json(
      json_path,
      paste0(block_query, "/", i, "/p/l"),
      required = FALSE
    )
    # no more blocks
    if (json_missing(label_i)) {
      break
    }
    # found the right one?
    if (identical(label_i, label)) {
      return(i)
    }
    i <- i + 1L
  }
  cli_abort(
    "searched {i+1} CBlockData entr{?y/ies} but one with label {.val {label}} was not found in {col_green{block_query}}"
  )
}
