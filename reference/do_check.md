# Perform a check

Checks whether variables fulfill a specific test.

## Usage

``` r
do_check(
  df,
  v_vars,
  check,
  label_check,
  template_ok = "all variables are {label_check}",
  template_fail = "{var} is not {label_check}"
)
```

## Arguments

- df:

  a dataframe.

- v_vars:

  character vector of variables on which to apply the test.

- check:

  a function to apply to the \`vars\`.

- label_check:

  character string. Text describing the test to pass.

- template_ok:

  character string. Text to display when a test is passed by a variable.

- template_fail:

  character string. Text to display when a test is not passed by a
  variable.

## Value

List containing the results of the check (checks), and a tibble of
status and message for each test (messages). The list of messages in the
result contains a single line if the test passed, or if a test failed
for one or more variables, a line for each failure.

## Examples

``` r
data(df_pa)
do_check(df = df_pa,
         v_vars = c("u_pfs", "u_pd"),
         check = ~ .x >= 0,
         label_check = "positive"
         )
#> $check
#> u_pfs  u_pd 
#>  TRUE  TRUE 
#> 
#> $messages
#> # A tibble: 1 × 2
#>   ok    message                   
#>   <lgl> <glue>                    
#> 1 TRUE  all variables are positive
#> 
```
