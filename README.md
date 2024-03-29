
## Install anyhoodle R package

``` r
devtools::install_git(
  url = "https://github.com/barnzilla/anyhoodle.git",
  force = TRUE
)
```

## Import dependencies

``` r
library(anyhoodle)
```

## Get descriptive stats and render bar plots

``` r
# Render bar plot of means for multiple numeric vectors
get_descriptives(
  df = mtcars,
  outcome_variable = c(mpg, disp, hp)
) %>%
render_bar_plot

# Render bar plot of means for multiple numeric vectors by a grouping variable
get_descriptives(
  df = mtcars %>%
    mutate(cyl = factor(cyl)),
  outcome_variable = c(mpg, disp),
  grouping_variable = cyl
) %>%
render_bar_plot

# Render bar plot of props for a non-numeric vector
get_descriptives(
  df = mtcars %>%
    mutate(cyl = factor(cyl)),
  outcome_variable = cyl
) %>%
render_bar_plot

# Render bar plot of props for multiple non-numeric vectors
get_descriptives(
  df = mtcars %>%
    mutate(
      cyl = factor(cyl),
      cyl2 = cyl
    ),
  outcome_variable = c(cyl, cyl2)
) %>%
render_bar_plot
```
