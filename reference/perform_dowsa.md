# Perform deterministic one-way sensitivity analyses using probabilistic inputs and outputs.

This function performs the deterministic one-way sensitivity analyses
(DOWSA) using probabilistic inputs and outputs for the health economic
model developed to test the package. The outcome of the DOWSA is the
incremental net monetary benefit.

## Usage

``` r
perform_dowsa(df, vars, wtp = 120000)
```

## Arguments

- df:

  a dataframe. This dataframe contains the probabilistic inputs and
  outputs of the health economic model.

- vars:

  a vector of strings. Contains the name of the variables for which to
  perform the deterministic one-way sensitivity analysis.

- wtp:

  numeric. The willingness to pay per QALY in euros. Default is 120,000
  euros per QALY.

## Value

A dataframe. The outcome of the deterministic one-way sensitivity
analyses is the iNMB by default.

## Examples

``` r
# Perform the deterministic one-way sensitivity analyses for a selection of parameters
if (FALSE) { # \dontrun{
data(df_pa)
df_res_dowsa <- perform_dowsa(df = df_pa,
                              vars = c("rr", "c_pfs"))
                              } # }
```
