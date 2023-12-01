## code to prepare `DATASET` dataset goes here

discovery <- readr::read_csv(file = "./data-raw/admiral-lookup-book.csv") |>
  dplyr::arrange(!dataset %in% "ADSL", dataset, variable)

attr(discovery$dataset, "label") <- "Data Set"
attr(discovery$dataset_type, "label") <- "Data Set Type" # ADSL, BDS, OCCDS
attr(discovery$variable, "label") <- "Variable"
attr(discovery$variable_label, "label") <- "Variable Label"
attr(discovery$package, "label") <- "Admiral Package"
attr(discovery$fn, "label") <- "Function"
attr(discovery$fn_url, "label") <- "Function URL"
attr(discovery$resource1_text, "label") <- "Resource Link Text"
attr(discovery$resource1_url, "label") <- "Resource URL"

if (discovery |> dplyr::mutate(.by = c("dataset", "variable"), N = dplyr::n()) |> dplyr::pull(N) |> max() > 1L) {
  cli::cli_abort(c("x" = "DUPLICATE VARIABLES FOUND!"))
}

usethis::use_data(discovery, overwrite = TRUE)
