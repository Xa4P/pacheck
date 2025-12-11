# Generate probabilistic model inputs.

This function generates the probabilistic model inputs for the example
health economic model developed to test the functionalities of the
package.

## Usage

``` r
generate_pa_inputs(n_sim = 10000, sd_var = 0.2, seed_num = 452)
```

## Arguments

- n_sim:

  integer. Number of probabilistic value to draw for each model input.
  Default is 10,000.

- sd_var:

  numeric. Determines the standard error of the mean to use for the
  normal distributions when the standard error not known. Default is 0.2
  (20%).

- seed_num:

  integer. The seed number to use when drawing the probabilistic values.
  Default is 452.

## Value

A dataframe. A description of the variables of the returned dataframe is
available in the documentation of the
[`df_pa`](https://xa4p.github.io/pacheck/reference/df_pa.md) dataframe.

## Examples

``` r
# Generating deterministic model inputs and storing them in an object.
df_inputs_prob <- generate_pa_inputs()
```
