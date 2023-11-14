#' Interactive Admiral Function Discovery
#'
#' Interactively explore admiral functions and the ADaM data sets and variables
#' they help create.
#'
#' @param type type of table to return
#' @return interactive gt table
#' @export
#'
#' @examplesIf FALSE
#' interactive_discovery("gt")
#' interactive_discovery("reactable")
interactive_discovery <- function(type = c("gt", "reactable")) {
  # check inputs ---------------------------------------------------------------
  type <- match.arg(type)

  # create interactive table ---------------------------------------------------
  switch(
    type,
    "gt" = .gt_discovery(),
    "reactable" = .reactable_discovery()
  )
}

.reactable_discovery <- function() {
  admiraldiscovery::discovery |>
    dplyr::select(-c("package", "fn", "resource1_text")) |>
    reactable::reactable(
      columns = list(
        dataset = reactable::colDef(
          name = attr(admiraldiscovery::discovery$dataset, 'label'),
          maxWidth = 100
        ),
        variable = reactable::colDef(
          name = attr(admiraldiscovery::discovery$variable, 'label'),
          maxWidth = 100
        ),
        variable_label = reactable::colDef(
          name = attr(admiraldiscovery::discovery$variable_label, 'label')
        ),
        fn_url =  reactable::colDef(
          name = attr(admiraldiscovery::discovery$fn, 'label'),
          cell = function(value, index) {
            htmltools::tags$a(
              href = value,
              target = "_blank",
              glue::glue("{admiraldiscovery::discovery$package[index]}::{admiraldiscovery::discovery$fn[index]}()")
            )
          },
          width = 150
        ),
        resource1_url =  reactable::colDef(
          name = "Resource",
          cell = function(value, index) {
            htmltools::tags$a(
              href = value,
              target = "_blank",
              glue::glue("{admiraldiscovery::discovery$resource1_text[index]}")
            )
          },
          width = 150
        )
      ),
      searchable = TRUE,
      filterable = TRUE,
      defaultPageSize = 20,
      compact = TRUE,
      style = list(fontFamily = "Work Sans, sans-serif", fontSize = "0.875rem"),
    )
}

.gt_discovery <- function() {
  admiraldiscovery::discovery |>
    dplyr::mutate(
      function_link = glue::glue("[`{package}::{fn}()`]({fn_url})"),
      .after = "fn_url"
    ) |>
    dplyr::mutate(
      resource1_link = glue::glue("[{resource1_text}]({resource1_url})")
    ) |>
    gt() |>
    cols_hide(columns = c("fn", "fn_url", "package", "dataset_type",
                          "resource1_text", "resource1_url")) |>
    cols_label(
      function_link = "Function",
      resource1_link = "Resources"
    ) |>
    fmt_markdown(columns = c("function_link", "resource1_link")) |>
    sub_missing(missing_text = "")  |>
    cols_width(c(dataset, dataset_type, variable) ~ pct(8)) |>
    cols_align(align = "left") |>
    opt_interactive(
      use_search = TRUE,
      use_filters = TRUE,
      use_resizers = TRUE,
      use_highlight = TRUE,
      use_compact_mode = TRUE,
      use_page_size_select = TRUE,
      page_size_default = 10,
    )
}
