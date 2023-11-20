test_that("Check Valid URLs", {
  valid_url <- function(url_in, t = 2) {
    con <- url(url_in)
    check <- suppressWarnings(try(open.connection(con, open = "rt", timeout = t), silent = T)[1])
    suppressWarnings(try(close.connection(con), silent = TRUE))
    ifelse(is.null(check), TRUE, FALSE)
  }

  expect_true(
    discovery |>
      dplyr::select(dplyr::ends_with("_url")) |>
      unlist() |>
      unname() |>
      lapply(valid_url) |>
      unlist() |>
      all()
  )
})
