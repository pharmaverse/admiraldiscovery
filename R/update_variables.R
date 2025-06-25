## update_variables.R

## ------------------------  update_variables.R
## STATUS:    Do not review;  this is file is placeholder
## GOAL:      Using JSON file in pharamaverseadam as source,
##            update the .csv file admiral-lookup-book.csv in admiraldiscovery.
## Approach:  simple as possible.   Any benefit to `tibblify` package?
## ------------------------  


library(jsonlite)
library(pak)
library(tibblify) # an option

## ------------------------  target

# URL
## "https://github.com/pharmaverse/admiraldiscovery/blob/main/inst/admiral-lookup-book.csv" 

csv_file  <- "https://raw.githubusercontent.com/pharmaverse/admiraldiscovery/refs/heads/main/inst/admiral-lookup-book.csv"

target  <- read.csv(csv_file)
dim(target)   #424 x 9

# OR, 
library(readr)
target2  <- readr::read_csv(csv_file)

## ------------------------  source


## URL
##"https://github.com/pharmaverse/pharmaverseadam/blob/main/inst/extdata/adams-specs.json"

json_file <- "https://raw.githubusercontent.com/pharmaverse/pharmaverseadam/refs/heads/main/inst/extdata/adams-specs.json"

source_json  <- fromJSON(json_file)
source_json
v  <- source_json$variables
typeof(v)
length(source_json$variables)


source_tib  <- tibblify(json_source)

