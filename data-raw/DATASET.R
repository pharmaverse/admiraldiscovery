## code to prepare `DATASET` dataset goes here

discovery <- readr::read_csv(file = "./inst/admiral-lookup-book.csv") |>
  dplyr::arrange(!dataset %in% "ADSL", dataset, variable)

attr(discovery$dataset, "label") <- "Data Set"
attr(discovery$dataset_type, "label") <- "Data Set Type" # ADSL, BDS, OCCDS
attr(discovery$variable, "label") <- "Variable/Parameter"
attr(discovery$variable_label, "label") <- "Description"
attr(discovery$package, "label") <- "Admiral Package"
attr(discovery$fn, "label") <- "Function"
attr(discovery$fn_url, "label") <- "Function URL"
attr(discovery$resource1_text, "label") <- "Resource Link Text"
attr(discovery$resource1_url, "label") <- "Resource URL"

usethis::use_data(discovery, overwrite = TRUE)
