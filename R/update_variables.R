## update_variables.R

## ------------------------  update_variables.R
## STATUS:    DRAFT.  If ok, will roxygenate, lint, standardize.... 
## GOAL:      Using JSON file in pharamaverseadam as source,
##            update the .csv file admiral-lookup-book.csv in admiraldiscovery.
## ------------------------  


library(jsonlite)
library(pak)
library(readr)

## ------------------------  target (file to update)


csv_file  <- "https://raw.githubusercontent.com/pharmaverse/admiraldiscovery/refs/heads/main/inst/admiral-lookup-book.csv"


target  <- readr::read_csv(csv_file)
## ------------------------  source

json_file <- "https://raw.githubusercontent.com/pharmaverse/pharmaverseadam/refs/heads/main/inst/extdata/adams-specs.json"

source_json  <- fromJSON(json_file)
source <- as_tibble(source_json$Variables)

# Change keys in source to match the keys used target.
new = dplyr::rename(source, dataset=Dataset, variable=Variable, variable_label = Label)
new2 = new |> dplyr::select(dataset, variable, variable_label)


# ------------------------  update

res = dplyr::rows_update(x=target, y=new2, by = c("dataset", "variable"), unmatched = "ignore")
