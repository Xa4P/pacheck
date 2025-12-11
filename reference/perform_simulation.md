# Perform the health economic simulation.

This function performs the simulation of the health economic model
developed to test the functionalities of the package.

## Usage

``` r
perform_simulation(l_params)
```

## Arguments

- l_params:

  list. List of inputs of the health economic model

## Value

A vector. This vector contains the (un)discounted intermediate and final
outcomes of the health economic model.

## Examples

``` r
# Perform the simulation using the deterministic model inputs
l_inputs_det <- generate_det_inputs()
v_results_det <- perform_simulation(l_inputs_det)
```
