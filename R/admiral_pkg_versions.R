#' Report Package Versions
#'
#' Function returns a string listing the packages and their versions.
#'
#' @export
#' @examples
#' admiral_pkg_versions()
admiral_pkg_versions <- function() {
  c("admiral", "admiralonco", "admiralophtha", "admiralvaccine") |>
    lapply(\(x) glue::glue("{x} v{packageVersion(x)}")) |>
    unlist() |>
    glue::glue_collapse(sep = ", ", last = ", and ")
}

