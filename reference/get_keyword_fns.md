# Find Functions with Keyword

`get_admiral_deprecated()`: Returns tibble of all deprecated functions
in the admiral, admiralonco, admiralophtha, and admiralvaccine packages.

`get_admiral_superseded()`: Returns tibble of all superseded functions
in the admiral, admiralonco, admiralophtha, and admiralvaccine packages.

`get_fns_with_keyword()`: Returns a character vector of functions that
have the passed keyword in the help file. For example, this function can
be used to find all deprecated or superseded functions in the admiral
universe, as admiral package include `#' @keywords deprecated` or
`#' @keywords superseded` in the function's roxygen2 comments.

## Usage

``` r
get_admiral_deprecated()

get_admiral_superseded()

get_fns_with_keyword(package, keyword, lib.loc = NULL)
```

## Arguments

- package:

  a character string naming an installed package.

- keyword:

  string of the keyword to identify

- lib.loc:

  a character vector of directory names of R libraries, or `NULL`. The
  default value of `NULL` corresponds to all libraries currently known.
  The specified library trees are used to search for `package`.

## Value

a character vector of function names

## Examples

``` r
get_admiral_deprecated()
#> # A tibble: 18 × 2
#>    package       fn                         
#>    <chr>         <chr>                      
#>  1 admiral       call_user_fun              
#>  2 admiral       date_source                
#>  3 admiral       derive_param_extreme_record
#>  4 admiral       derive_var_dthcaus         
#>  5 admiral       derive_var_extreme_dt      
#>  6 admiral       derive_var_extreme_dtm     
#>  7 admiral       derive_var_merged_summary  
#>  8 admiral       dthcaus_source             
#>  9 admiral       get_summary_records        
#> 10 admiralonco   date_source                
#> 11 admiralonco   derive_param_bor           
#> 12 admiralonco   derive_param_clinbenefit   
#> 13 admiralonco   derive_param_confirmed_bor 
#> 14 admiralonco   derive_param_confirmed_resp
#> 15 admiralonco   derive_param_response      
#> 16 admiralonco   filter_pd                  
#> 17 admiralophtha derive_var_bcvacritxfl     
#> 18 admiralophtha derive_var_bcvacritxfl_util
get_admiral_superseded()
#> # A tibble: 0 × 2
#> # ℹ 2 variables: package <chr>, fn <chr>
get_fns_with_keyword(package = "admiral", keyword = "superseded")
#> NULL
```
