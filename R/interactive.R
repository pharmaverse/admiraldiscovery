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
  temp <- admiraldiscovery::discovery |>
    dplyr::left_join(
      get_admiral_deprecated() |> dplyr::mutate(deprecated = TRUE),
      by = c("package", "fn")
    ) |>
    dplyr::left_join(
      get_admiral_superseded() |> dplyr::mutate(superseded = TRUE),
      by = c("package", "fn")
    ) |>
    dplyr::select(-c("package", "fn", "resource1_text", "dataset_type"))

  temp |>
    reactable::reactable(
      rowStyle = function(index) {
        if (is.na(temp[index, "superseded"]) && is.na(temp[index, "deprecated"])) list(background = '#ffffff')
        else if (temp[index, "superseded"] == TRUE) list(background = '#ffffba')
        else if (temp[index, "deprecated"] == TRUE) list(background = '#ffb3ba')
      },

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
        ),
        deprecated = reactable::colDef(show = FALSE),
        superseded = reactable::colDef(show = FALSE)
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
    dplyr::left_join(
      get_admiral_deprecated() |> dplyr::mutate(deprecated = TRUE),
      by = c("package", "fn")
    ) |>
    dplyr::left_join(
      get_admiral_superseded() |> dplyr::mutate(superseded = TRUE),
      by = c("package", "fn")
    ) |>
  dplyr::mutate(
    function_link = glue::glue("[`{package}::{fn}()`]({fn_url})"),
    .after = "fn_url"
  ) |>
    dplyr::mutate(
      resource1_link = glue::glue("[{resource1_text}]({resource1_url})")
    ) |>
    gt::gt() |>
    gt::cols_hide(columns = c("fn", "fn_url", "package", "dataset_type",
                              "resource1_text", "resource1_url",
                              "deprecated", "superseded")) |>
    gt::cols_label(
      function_link = "Function",
      resource1_link = "Resources"
    ) |>
    # color deprecated/superseded functions
    gt::tab_style(
      style = gt::cell_fill(color = '#ffb3ba'),
      locations = gt::cells_body(columns = gt::everything(), rows = .data$deprecated)
    ) |>
    gt::tab_style(
      style = gt::cell_fill(color = '#ffffba'),
      locations = gt::cells_body(columns = gt::everything(), rows = .data$superseded)
    ) |>
    gt::fmt_markdown(columns = c("function_link", "resource1_link")) |>
    gt::sub_missing(missing_text = "")  |>
    gt::cols_width(c(dataset, dataset_type, variable) ~ gt::pct(8)) |>
    gt::cols_align(align = "left") |>
    gt::opt_interactive(
      use_search = TRUE,
      use_filters = TRUE,
      use_resizers = TRUE,
      use_highlight = TRUE,
      use_compact_mode = TRUE,
      use_page_size_select = TRUE,
      page_size_default = 10,
    )
}
