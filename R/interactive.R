#' Interactive Admiral Function Discovery
#'
#' Interactively explore admiral functions and the ADaM data sets and variables
#' they help create.
#'
#' @return interactive gt table
#' @export
#'
#' @examplesIf interactive()
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
    dplyr::select(-c("package", "fn")) |>
    reactable::reactable(
      columns = list(
        dataset = reactable::colDef(
          name = attr(admiraldiscovery::discovery$dataset, 'label'),
          maxWidth = 100
        ),
        dataset_type = reactable::colDef(
          name = attr(admiraldiscovery::discovery$dataset_type, 'label'),
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
        adam_ig_reference = reactable::colDef(
          name = attr(admiraldiscovery::discovery$adam_ig_reference, 'label')
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
    gt() |>
    cols_hide(columns = c("fn", "fn_url", "package")) |>
    cols_label(function_link = "Function") |>
    fmt_markdown(columns = "function_link") |>
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
      page_size_default = 25,
    )
}
