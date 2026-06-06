# pretty formatting ===========

# convert bytes to pretty text
bytes_to_text <- function(bytes) {
  bytes |>
    purrr::map_chr(
      ~ if (is.na(.x)) {
        return(NA_character_)
      } else {
        .x |> structure(class = "object_size") |> format(units = "auto")
      }
    )
}

# predefined metric prefixes for number formatting
.metric_prefixes <- set_names(
  c(1e-15, 1e-12, 1e-9, 1e-6, 1e-3, 1, 1e3, 1e6, 1e9, 1e12),
  c("f", "p", "n", stringi::stri_encode("\U00B5"), "m", "", "k", "M", "G", "T")
)

# convert numbers to pretty text
numbers_to_text <- function(
  x,
  signif = 3,
  format = "%s%s",
  consider_zero = 1e-18,
  trim_ws = TRUE
) {
  stopifnot(!missing(x) && is.numeric(x))
  if (is_empty(x)) {
    return(character(0))
  }
  # best prefix
  prefix <- x |>
    abs() |>
    purrr::map_int(
      ~ max(1, which(.x / .metric_prefixes >= 1))
    )

  # deal with zeros
  is_zero <- abs(x) < consider_zero
  x[is_zero] <- 0
  prefix[is_zero] <- which(names(.metric_prefixes) == "")

  # convert
  out <- signif(
    x / .metric_prefixes[prefix],
    digits = signif
  ) |>
    sprintf(fmt = format, names(.metric_prefixes)[prefix])

  # special cases?
  is_special = is.na(x) | is.infinite(x) | is.nan(x)
  out[is_special] <- as.character(x[is_special])

  # trim whitespaces?
  if (trim_ws) {
    out <- trimws(out)
  }

  # return
  return(out)
}
