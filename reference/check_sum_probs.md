# Check sum probabilities

This function checks whether the sum of user-defined variables
representing probabilities is below or equal to 1 for each iteration of
the probabilistic inputs.

## Usage

``` r
check_sum_probs(..., df, digits = NULL, check = "lower", max_view = 100)
```

## Arguments

- ...:

  character vector. This character vector contains the name of the
  variables of which the sum will be checked.

- df:

  a dataframe.

- digits:

  numeric. Define the number of digits at which the sum of probabilities
  should be rounded.

- check:

  logical. Define which test to perform."lower" tests whether the sum of
  the selected variables is lower than or equal to 1 for each iteration.
  "equal" tests whether the sum of the selected variables is equal to 1
  for each iteration. Default is "lower".

- max_view:

  numeric. Determines the number of iterations to display which do not
  fulfill the test Default is 100.

## Value

A text indicating whether the sum of the probabilities is belor and/or
eual to one or indicating in which iteration that is not the case.

## Examples

``` r
# Checking whether the sum of the two probabilities is lower than or equal to 1
check_sum_probs("p_pfspd", "p_pfsd", df = df_pa, check = "lower")
#> [1] "The sum of probabilities in all iterations is lower or equal to 1"

# Checking the sum of the two probabilities equals 1 using a vector to select them,
# Rounding off to two digits, and extending the number of iterations to display to 250.
check_sum_probs(c("p_pfspd", "p_pfsd"), df = df_pa, digits = 2, check = "equal", max_view = 250)
#> [1] "The sum of probabilities is different than 1 in the following iterations: 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109, 110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 123, 124, 125, 126, 127, 128, 129, 130, 131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146, 147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178, 179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210, 211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226, 227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246, 247, 248, 249, 250"
```
