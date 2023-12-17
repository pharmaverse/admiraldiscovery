test_that("discovery DATASET is consistent", {
  # the number of rows in the CSV file are the same as the discovery dataset in the pkg
  expect_equal(
    discovery |> nrow(),
    fs::path_package("admiraldiscovery", "admiral-lookup-book.csv") |>
      readr::read_csv(show_col_types = FALSE) |>
      nrow()
  )

  # there are no duplicated variable names within a data set
  expect_false(
    discovery |>
      dplyr::select(dataset, variable) |>
      anyDuplicated() |>
      as.logical()
  )

  # the dataset type is consistent for every dataset
  expect_true(
    discovery |>
      dplyr::mutate(
        .by = "dataset",
        consistent_type = dataset_type == dataset_type[1]
      ) |>
      dplyr::pull(consistent_type) |>
      all()
  )

  # all columns are labeled
  expect_true(
    lapply(
      discovery,
      function(x) {
        attr(x, "label") |>
          rlang::is_string()
      }
    ) |>
      unlist() |>
      all()
  )
})

test_that("Warn for Deprecated/Supersded Functions", {
  expect_output({
    df_superseded <-
      dplyr::inner_join(
        get_admiral_superseded(),
        discovery,
        by = dplyr::join_by(package, fn)
      )
    if (nrow(df_superseded) > 0L) {
      fns <- glue::glue_data(df_superseded, "{package}::{fn}")
      cli::cli_warn("Superseded functions are present in vignettes! {.val {fns}}")
    }
    print(df_superseded)
  })

  expect_output({
    df_deprecated <-
      dplyr::inner_join(
        get_admiral_deprecated(),
        discovery,
        by = dplyr::join_by(package, fn)
      )
    if (nrow(df_deprecated) > 0L) {
      fns <- glue::glue_data(df_deprecated, "{package}::{fn}")
      cli::cli_warn("Deprecated functions are present in vignettes! {.val {fns}}")
    }
    print(df_deprecated)
  })

})


test_that("Check Valid URLs", {
  valid_url <- function(url_in, t = 2) {
    con <- url(url_in)
    check <- suppressWarnings(try(open.connection(con, open = "rt", timeout = t), silent = TRUE)[1])
    suppressWarnings(try(close.connection(con), silent = TRUE))
    ret <- ifelse(is.null(check), TRUE, FALSE)
    if (isFALSE(ret)) {
      cli::cli_inform("Invalid URL: {.url {url_in}}")
    }

    ret
  }

  lst_valid_urls <-
    discovery |>
    dplyr::select(dplyr::ends_with("_url")) |>
    unlist() |>
    unname() |>
    unique() |>
    lapply(valid_url)

  expect_true(
    lst_valid_urls |>
      unlist() |>
      all()
  )
})
