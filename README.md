
<!-- README.md is generated from README.Rmd. Please edit that file -->

# strictlyr <img src="man/figures/logo.png" align="right" height=230/>

<!-- badges: start -->

![](https://img.shields.io/badge/cool-useless-green.svg)
![](http://img.shields.io/badge/very-experimental-orange.svg)
<!-- badges: end -->

The goal of `strictlyr` is to provide functions that are stricter about
violations of some common assumptions during data manipulation.

The key issues which this package will initially focus on handling:

  - datasets in the environment should not have groups
  - conditions and predicates (for example in `filter()` and
    `if_else()`) should never evaluate to an `NA` value.
  - in a `case_when()` the “looseness” of the matching can mean that it
    is easy to make mistakes
  - there is a need for a more specialised `left_join()` which enforces
    some conditions on the RHS

To address these issues, `strictlyr` will:

1.  include drop-in replacement functions which check for issues
    internally and raise errors, warnings or messages.
2.  include alternate versions of dplyr functions which enforce
    particular assumptions

## Installation + Usage

You can install `strictlyr` from
[github](https://github.com/coolbutuseless/strictlyr) with:

``` r
remotes::install_github("coolbutuseless/strictlyr")
```

``` r
# Always load `dplyr` first
library(dplyr    , warn.conflicts = FALSE)
library(strictlyr, warn.conflicts = FALSE)
```

## Assumption: data in the environment should not have groups.

In 100% of the code that I write, I do not want data.frames with groups
in the global environment.

Every `group_by()` I write is paired with an immediate `ungroup()` after
I’ve done what needs doing. If I ever forget to `ungroup()` then this is
a mistake that will lead to data issues later in the script.

`strictlyr` includes a drop-in replacement for the pipe operator which
checks if the input or output data is grouped.

``` r
res1 <- mtcars %>%
  group_by(cyl) %>%
  mutate(mpg = max(mpg))
#> Error: The end result of this operation still has groups - did you mean to call `ungroup()` as well?
```

This error may be configured by setting either of the following options:

  - setting `options(STRICTLYR_LOG = 'quiet')` - make all `strictlyr`
    functions quiet
  - setting `options(STRICTLYR_PIPE = 'quiet')` - make only the pipe
    quiet

Possible values for `STRICTLYR_PIPE` are **“stop”**, **“warning”**,
**“message”**, and
**“quiet”**.

``` r
options(STRICTLYR_PIPE = 'quiet') # Suppress `strictlyr` output for the pipe

res1 <- mtcars %>%
  group_by(cyl) %>%
  mutate(mpg = max(mpg))
```

## Assumption: Predicates in a `filter()` operation should never produce `NA`s

An `NA` as a result of a predicate in a `filter()` statement is almost
always an indication that I have made a mistake e.g. I don’t understand
my data, I’ve made an earlier data handling error, or new data has
violated earlier assumptions.

To be clear: having `NA` values in the actual dataset is fine, but
having `NA` as the result of a filter predicate is not.

An example of a type of error that can occur if a wild and unexpected
`NA` appears in your dataset is included below. In this scenario, `df$x`
previously never contained `NA` values, but a data update violated this
assumption. Code that previously worked now silently drops any row where
`x == NA`\!

``` r
# Dataset with 3 rows
test_df <- data.frame(x = c(1, NA, 3), y = c(4, 5, 6))

# split the data
low_df  <- test_df %>% filter(x <  2)
high_df <- test_df %>% filter(x >= 2)

# calculate something on the separate datasets and then re-combine.
# Now there are only 2 rows in the data!
dplyr::bind_rows(low_df, high_df)
#>   x y
#> 1 1 4
#> 2 3 6
```

## Assumption: The condition in an `if_else()` statement should never produce `NA`s

An `NA` as a result of the condition in an `if_else` statement is almost
always an indication that I have made a mistake e.g. I don’t understand
my data, I’ve made an earlier data handling error, or new data has
violated earlier assumptions.

An example of a type of error that can occur if a wild and unexpected
`NA` appears in your dataset is included below. In this scenario, `x`
previously never contained `NA` values, but a data update violated this
assumption. Code that previously worked now changes the total count of

``` r
# A rogue 'NA' has appeared in the data where there never was before.
x <- c(1, 2, NA)

size <- if_else(x < 2, 'small', 'large')

N_small <- length(size[size == 'small'])
N_large <- length(size[size == 'large'])

# Now have a erroneous count
N_small + N_large
#> [1] 4
```

## Assumption: For `case_when()`, each input element should match only 1 rule.

In the following `case_when()` code the output is a pretty awful due to
a combination of typos, rule misspecification, and `NA` values.

I want a `case_when()` which avoids some easy errors i.e. it should:

  - tell me that there are multiple rules which match when the input is
    ‘dog’
  - disallow the bare `TRUE` rule so that `catt` would be picked up as a
    typo rather than classified as a reptile.
  - somehow stop the `NA` value being classed as a reptile. An easy
    solution would again be to disallow the bare `TRUE` rule.

<!-- end list -->

``` r
animal <- c('cat', 'dog', 'dogs', 'snake', NA)

case_when(
  animal == 'catt'          ~ 'mammal',
  animal == 'dog'           ~ 'mammal', 
  startsWith(animal, 'dog') ~ "best friend",
  TRUE                      ~ "reptile"
)
#> [1] "reptile"     "mammal"      "best friend" "reptile"     "reptile"
```

`case_when()` applies the first matching rule that it finds, and this is
often very useful. So to the match the desired strict behaviour, there
would need to be alternate function called `strict_case_when()`. See
this post for more discussion:
<https://coolbutuseless.github.io/2018/09/06/strict-case_when/>

## Assumption: In a `left_join()` operation, the RHS should have (at most) one row matching each row in the LHS

In the majority of `left_join()` calls, I expect (at most) one match in
the RHS dataset. In these types of `left_join()` calls, wheee there are
multiple matching rows in the RHS, I would prefer an error rather than
the propagation of duplicate rows.

``` r
# Expecting one measurement of weight and height per subject
# There is an erroneous duplicate height recorded for subject 2
weight <- data.frame(ID = 1:2, wt = c(10, 20))
height <- data.frame(ID = c(1, 2, 2, 3), ht = c(20, 21, 21, 22))

# Now the total measurements data has a duplicate row too!
measurements <- weight %>% left_join(height, by = 'ID')
measurements
#>   ID wt ht
#> 1  1 10 20
#> 2  2 20 21
#> 3  2 20 21
```

The `left_join` is quite a powerful operator, and restricting the RHS to
one matching row would cripple its usefulness in general. So I think
there should be alternate function: `strict_left_join()`

See other discussion about `left_joins()` and multiple matching rows in:

  - <https://github.com/tidyverse/dplyr/issues/2278>

  - <https://github.com/tidyverse/dplyr/issues/1619>

  -   - add post-join diagnostics to the output.
          - <https://github.com/tidyverse/dplyr/issues/2183>  
          - <https://github.com/tidyverse/dplyr/issues/3231>

  - <https://github.com/tidyverse/dplyr/issues/1792>

## Guidelines for function design within `strictlyr`

Drop-in replacement functions should

  - by default, raise an error when assumptions are violated.
  - use `options()` to configure output behaviour when assumptions are
    violated. i.e.  ‘error’, ‘warn’, ‘message’ or ‘quiet’
  - if the output is set to ‘quiet’, then the behaviour of the drop-in
    replacement should be indistinguishable from the original function.
  - output behaviour should be configurable both globally and
    per-function.

New/alternate functions should

  - output behaviour is not governed by setting `options()`
  - have a `strict_` prefix. e.g. `strict_filter()` would be an
    alternative to `filter()`
