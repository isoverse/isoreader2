# convert bytes to text
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
