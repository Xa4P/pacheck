# Perform the health economic simulation using partitioned survival model.

This function performs the simulation of the partitioned survival health
economic model developed to test the functionalities of the package.

## Usage

``` r
perform_simulation_psm(l_params, min_fct = TRUE)
```

## Arguments

- l_params:

  list. List of inputs of the health economic model.

- min_fct:

  logical. Should a minimum function be used to ensure PFS remains lower
  than OS? Default is TRUE.

## Value

A vector. This vector contains the (un)discounted intermediate and final
outcomes of the health economic model.

## Examples

``` r
# Perform the simulation using the deterministic model inputs
l_inputs_det <- generate_det_inputs()
v_results_det <- perform_simulation_psm(l_inputs_det)
#> Error in eval(substitute(expr), data, enclos = parent.frame()): object 'shape_weib_os' not found
```
