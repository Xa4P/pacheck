# Validate metamodels

Validate metamodels

## Usage

``` r
validate_metamodel(
  model = NULL,
  method = NULL,
  partition = 1,
  folds = 1,
  show_intercept = FALSE,
  seed_num = 1,
  df_validate = NULL
)
```

## Arguments

- model:

  model object. Built using a function from the PACHECK package.

- method:

  character, validation method. Choices are: cross-validation
  ('cross_validation'), train-test split ('train_test_split'), or the
  user can input a new dataframe which will be used as the test-set
  ('new_test_set'). No default.

- partition:

  numeric. Value between 0 and 1 to determine the proportion of the
  observations to use to fit the metamodel. Default is 1 (fitting the
  metamodel using all observations).

- folds:

  numeric. Number of folds for the cross-validation. Default is 1 (so an
  error occurs when not specifying this argument when cross-validation
  is chosen).

- show_intercept:

  logical. Determine whether to show the intercept of the perfect
  prediction line (x = 0, y = 0). Default is FALSE.

- seed_num:

  numeric. Determine which seed number to use to split the dataframe in
  fitting and validation sets.

- df_validate:

  dataframe. The dataframe to be used for validating the model. By
  default the dataframe used when building the model is used.

## Value

.........................

## Examples

``` r
#Validating meta model with two variables using the probabilistic data, using cross-validation.
data(df_pa)
lm_fit = fit_lm_metamodel(df = df_pa,
                 y_var = "inc_qaly",
                 x_vars = c("p_pfsd", "p_pdd")
                 )

validate_metamodel(model = lm_fit,
                 method = "cross_validation",
                 folds = 5
                 )
#> $stats_validation
#>             Statistic Value (method: cross-validation)
#> 1           R-squared                            0.478
#> 2 Mean absolute error                            0.087
#> 3 Mean relative error                            3.100
#> 4  Mean squared error                            0.013
#> 
```
