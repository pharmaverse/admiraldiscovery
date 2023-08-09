## code to prepare `DATASET` dataset goes here

discovery <- readr::read_csv(file = "./data-raw/admiral-lookup-book.csv")

attr(discovery$dataset, 'label') <- "Data Set"
attr(discovery$dataset_type, 'label') <- "Data Set Type" # ADSL, BDS, OCCDS
attr(discovery$variable, 'label') <- "Variable"
attr(discovery$variable_label, 'label') <- "Variable Label"
attr(discovery$package, 'label') <- "Admiral Package"
attr(discovery$fn, 'label') <- "Function"
attr(discovery$fn_url, 'label') <- "Function URL"
attr(discovery$adam_ig_reference, 'label') <- "ADaM Implmentation Guide Reference"

usethis::use_data(discovery, overwrite = TRUE)
