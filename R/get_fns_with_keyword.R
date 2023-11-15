#' Find Functions with Keyword
#'
#' Returns a character vector of functions that have the passed keyword
#' in the help file. For example, this function can be used to find all
#' deprecated or superseded functions in the admiral universe, as admiral
#' package include `#' @keywords deprecated` or `#' @keywords superseded`
#' in the function's roxygen2 comments.
#'
#' @inheritParam tools::Rd_db
#' @param keyword string of the keyword to identify
#'
#' @return a character vector of function names
#' @export
#'
#' @examples
#' get_fns_with_keyword(package = "admiral", keyword = "superseded")
get_fns_with_keyword <- function(package, keyword, lib.loc = NULL) {
  browser()
  # parse the help files in package
  db <- tools::Rd_db(package = package)

  # data frame of all exported functions with indicator column indicating if
  # keyword appears in the help file
  df_all_fns <-
    dplyr::tibble(
      rd_file_name = names(db),
      rd_file_contents = lapply(rd_file_name, function(x) db[[x]] |> as.character() |> paste(collapse = "")),
      alias = lapply(rd_file_contents, extract_alias), # these are the function names
      has_keyword =
        lapply(rd_file_contents, function(x) stringr::str_detect(x, pattern = glue::glue("\\\\keyword\\{[[keyword]]\\}", .open = "[[", .close = "]]"))) |>
        unlist()
    )

  # return vector of all functions with matching keyword in help file
  df_all_fns |>
    dplyr::filter(has_keyword) |>
    dplyr::pull(alias) |>
    unlist(recursive = TRUE)
}


extract_alias <- function(x) {
  stringr::str_extract_all(x, "(?<=\\\\alias\\{)(.*?)(?=\\})")
}
