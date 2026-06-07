get_timebase <- function(interval, cutoff) {
  tb <- c(
    "years" = 365 * 86400,
    "months" = 30 * 86400,
    "weeks" = 7 * 86400,
    "days" = 86400,
    "h" = 3600,
    "m" = 60,
    "s" = 1,
    "ms" = 1e-3,
    "µs" = 1e-6,
    "ns" = 1e-9,
    "ps" = 1e-12,
    "fs" = 1e-15
  )
  base <- tb[interval >= cutoff * tb][1]
  if (is.na(base)) {
    base <- tail(tb, 1)
  }
  return(base)
}

# generates a pretty breaks duration function that expects seconds
breaks_pretty_duration <- function(n = 5, cutoff = 0.5) {
  # pretty breaks
  function(secs) {
    interval <- pretty(secs, n) |> diff() |> min(na.rm = TRUE)
    base <- interval |> get_timebase(cutoff)
    breaks <- pretty(secs / base, n) * base
    return(breaks)
  }
}

# generates a labels duration function that expects seconds
labels_duration <- function(cutoff = 0.5, short_format = FALSE) {
  sep <- if (short_format) "" else " "
  unit <- function(long, short) if (short_format) short else long
  # pretty labels
  function(secs) {
    interval <- diff(secs) |> min(na.rm = TRUE)
    base <- interval |> get_timebase(cutoff)
    if (names(base) %in% c("h", "m", "s")) {
      # various HH:MM:SS style formats
      h <- secs %/% 3600
      m <- secs %/% 60 %% 60
      s <- secs %% 60
      if (any(h > 0, na.rm = TRUE) && any(s > 0, na.rm = TRUE)) {
        # HH:MM:SS
        out <- sprintf(
          "%.0f:%02.0f:%02.0f%s%s",
          h,
          m,
          s,
          sep,
          unit("hours", "hr")
        )
      } else if (any(h > 0, na.rm = TRUE) && any(m > 0, na.rm = TRUE)) {
        # HH:MM
        out <- sprintf("%.0f:%02.0f%s%s", h, m, sep, unit("hours", "hr"))
      } else if (any(h > 0, na.rm = TRUE)) {
        # HH
        out <- sprintf("%.0f%s%s", h, sep, unit("hours", "hr"))
      } else if (any(m > 0, na.rm = TRUE) && any(s > 0, na.rm = TRUE)) {
        # MM:SS
        out <- sprintf("%.0f:%02.0f%s%s", m, s, sep, unit("min", "m"))
      } else if (any(m > 0, na.rm = TRUE)) {
        # MM
        out <- sprintf("%.0f%s%s", m, sep, unit("min", "m"))
      } else {
        # SS
        out <- sprintf("%s%s%s", signif(s, 3), sep, unit("secs", "s"))
      }
    } else {
      # sub-second units (ms, µs, ns, ...)
      out <- sprintf(
        "%s%s%s",
        format(signif(secs * 1 / base, 3), scientific = FALSE),
        sep,
        names(base)
      )
    }
    out[is.na(secs)] <- NA
    return(out)
  }
}
