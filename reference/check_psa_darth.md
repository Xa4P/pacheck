# Check PSA inputs & outputs

This function checks whether the value of variables remain between 0 and
1 for utility and probability inputs, and are strictly positive for
costs, hazard ratios, odds ratios, relative risks, and total outcomes of
each strategy.

## Usage

``` r
check_psa_darth(
  l_psa_darth,
  utility = "u_",
  costs = "c_",
  probs = "p_",
  rr = "rr_",
  hr = "hr_",
  or = "or_",
  exclude = NULL,
  v_outcome = c("effectiveness", "cost")
)
```

## Arguments

- l_psa_darth:

  a list of class 'psa' as obtained by the function
  \[dampack::make_psa_obj()\]

- utility:

  characters. String used at the start of the variables identifying
  utility inputs.

- costs:

  characters. String used at the start of the variables identifying cost
  inputs.

- probs:

  characters. String used at the start of the variables identifying
  probability inputs.

- rr:

  characters. String used at the start of the variables identifying
  relative risk inputs.

- hr:

  characters. String used at the start of the variables identifying
  hazard ratio inputs.

- or:

  characters. String used at the start of the variables identifying odds
  ratio inputs.

- exclude:

  vector of strings. Vector containing the name of the input variables
  not to include in the checks. Default is NULL, hence all variables
  from the 'parameters' dataframe are included.

- v_outcome:

  vector of strings. Vector containing the name of the output variables
  to include in the checks. Default values are 'effectiveness' and
  'cost'.

## Value

A matrix containing the input and output variables that have been
checked and the iterations wherein an erroneous value has been
identified.
