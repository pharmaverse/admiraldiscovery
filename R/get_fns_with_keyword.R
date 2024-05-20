#' Find Functions with Keyword
#'
#' @description
#'
#' `get_admiral_deprecated()`: Returns tibble of all deprecated functions in
#'   the admiral, admiralonco, admiralophtha, and admiralvaccine packages.
#'
#' `get_admiral_superseded()`: Returns tibble of all superseded functions in
#'   the admiral, admiralonco, admiralophtha, and admiralvaccine packages.
#'
#' `get_fns_with_keyword()`: Returns a character vector of functions that have
#'   the passed keyword in the help file. For example, this function can be used to find all
#'   deprecated or superseded functions in the admiral universe, as admiral
#'   package include `#' @keywords deprecated` or `#' @keywords superseded`
#'   in the function's roxygen2 comments.
#'
#' @inheritParams tools::Rd_db
#' @param keyword string of the keyword to identify
#'
#' @return a character vector of function names
#' @name get_keyword_fns
#'
#' @examples
#' get_admiral_deprecated()
#' get_admiral_superseded()
#' get_fns_with_keyword(package = "admiral", keyword = "superseded")
NULL

#' @rdname get_keyword_fns
#' @export
get_admiral_deprecated <- function() {
  c("admiral", "admiralonco", "admiralophtha", "admiralvaccine") |>
    lapply(
      function(x) {
        dplyr::tibble(
          package = x,
          fn = get_fns_with_keyword(package = x, keyword = "deprecated") %||% NA_character_
        )
      }
    ) |>
    dplyr::bind_rows() |>
    stats::na.omit()
}

#' @rdname get_keyword_fns
#' @export
get_admiral_superseded <- function() {
  c("admiral", "admiralonco", "admiralophtha", "admiralvaccine") |>
    lapply(
      function(x) {
        dplyr::tibble(
          package = x,
          fn = get_fns_with_keyword(package = x, keyword = "superseded") %||% NA_character_
        )
      }
    ) |>
    dplyr::bind_rows() |>
    stats::na.omit()
}

#' @rdname get_keyword_fns
#' @export
get_fns_with_keyword <- function(package, keyword, lib.loc = NULL) {
  # parse the help files in package
  db <- tools::Rd_db(package = package)

  # data frame of all exported functions with indicator column indicating if
  # keyword appears in the help file
  df_all_fns <-
    dplyr::tibble(
      rd_file_name = names(db),
      rd_file_contents = lapply(.data$rd_file_name, function(x) {
        db[[x]] |>
          as.character() |>
          paste(collapse = "")
      }),
      alias = lapply(.data$rd_file_contents, extract_alias), # these are the function names
      has_keyword =
        lapply(.data$rd_file_contents, function(x) stringr::str_detect(x, pattern = glue::glue("\\\\keyword\\{[[keyword]]\\}", .open = "[[", .close = "]]"))) |> #nolint
          unlist()
    )

  # return vector of all functions with matching keyword in help file
  df_all_fns |>
    dplyr::filter(.data$has_keyword) |>
    dplyr::pull(.data$alias) |>
    unlist(recursive = TRUE)
}


extract_alias <- function(x) {
  stringr::str_extract_all(x, "(?<=\\\\alias\\{)(.*?)(?=\\})")
}
