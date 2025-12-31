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
#> # A tibble: 8 × 2
#>   package fn                         
#>   <chr>   <chr>                      
#> 1 admiral call_user_fun              
#> 2 admiral date_source                
#> 3 admiral derive_param_extreme_record
#> 4 admiral derive_var_dthcaus         
#> 5 admiral derive_var_extreme_dt      
#> 6 admiral derive_var_extreme_dtm     
#> 7 admiral dthcaus_source             
#> 8 admiral get_summary_records        
get_admiral_superseded()
#> # A tibble: 8 × 2
#>   package       fn                         
#>   <chr>         <chr>                      
#> 1 admiralonco   derive_param_bor           
#> 2 admiralonco   derive_param_clinbenefit   
#> 3 admiralonco   derive_param_confirmed_bor 
#> 4 admiralonco   derive_param_confirmed_resp
#> 5 admiralonco   derive_param_response      
#> 6 admiralonco   filter_pd                  
#> 7 admiralophtha derive_var_bcvacritxfl     
#> 8 admiralophtha derive_var_bcvacritxfl_util
get_fns_with_keyword(package = "admiral", keyword = "superseded")
#> NULL
```
