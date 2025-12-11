# Generate summary statistics

This function generates summary statistics of input and output values of
a probabilistic analysis.

## Usage

``` r
generate_sum_stats(df, vars = NULL)
```

## Arguments

- df:

  a dataframe. This dataframe contains the probabilistic inputs and
  outputs of the health economic model.

- vars:

  a vector of strings. Contains the name of the variables to include in
  the summary statistics table. Default is NULL meaning all variables
  will be included.

## Value

A dataframe with summary statistics for the selected variables. The
returned summary statistics are:

- Mean

- Standard deviation

- 2.5th percentile

- 97.5th percentile

- Minimum

- Maximum

- Median

- Skewness

- Kurtosis

## Examples

``` r
# Generating summary data of all inputs
data(df_pa)
df_summary <- generate_sum_stats(df_pa)
```
