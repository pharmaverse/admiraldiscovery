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
names(target)
## ------------------------  source

json_file <- "https://raw.githubusercontent.com/pharmaverse/pharmaverseadam/refs/heads/main/inst/extdata/adams-specs.json"

source_json  <- fromJSON(json_file)
source <- as_tibble(source_json$Variables)

# Change keys in source to match the keys used target.
new = dplyr::rename(source, dataset=Dataset, variable=Variable, variable_label = Label)
new2 = new |> dplyr::select(dataset, variable, variable_label)


# ------------------------  update

res2 = dplyr::rows_update(x=target, y=new2, by = c("dataset", "variable"), unmatched = "ignore")

# result
diffdf::diffdf(target, res2)


# save
readr::write_csv(res2, "inst/admiral-lookup-book-NEW.csv")


## ------------------------  check row order
target |> head()
res2 |> head()

target |> tail()

unique(target$dataset)
sort(target$dataset)
table(target$dataset)


unique(res2$dataset)
sort(res2$dataset)
table(res2$dataset)

sink("inst/lookup.diff")
# original ds is target
# updated ds = res2
dim(target)   # 424 x 9
dim(res2)     # 424 x 9

diffdf::diffdf(target, res2)

sink()
readLines("inst/lookup.diff")

