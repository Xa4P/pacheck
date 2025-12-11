# Predict using a fitted metamodel

Predict using a fitted metamodel

## Usage

``` r
predict_metamodel(model = NULL, inputs = NULL, output_type = "vector")
```

## Arguments

- model:

  model object. Built using a function from the PACHECK package.

- inputs:

  dataframe or vector. When choosing a vector in the case of a
  three-variable model: the first, second, third, and fourth value
  represent the input for the first, second, third, and FIRST variable,
  respectively. Default gives the predictions based on the training
  data.

- output_type:

  character. Choose an output: 'dataframe', 'long_df' (long data.frame)
  or 'vector'.

## Value

returns a vector of the the predictions ('vector' output_type) or the
parameter values used for the predictions and the predictions
('dataframe' or 'long_df' output_type).

## Examples

``` r
#Making 3 predictions for a two-variable metamodel,
# using a vector as input, and yielding a dataframe as output.
data(df_pa)
lm_fit = fit_lm_metamodel(df = df_pa,
                 y_var = "inc_qaly",
                 x_vars = c("p_pfsd", "p_pdd")
                 )

vec = c(0.1,0.2,0.08,0.15,0.06,0.25)

predict_metamodel(model = lm_fit,
                 inputs = vec,
                 output_type = "dataframe"
                 )
#>   p_pfsd p_pdd predictions
#> 1   0.10  0.15   0.2256934
#> 2   0.20  0.06  -0.2006800
#> 3   0.08  0.25   0.3802208
```
