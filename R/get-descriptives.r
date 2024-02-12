#' Get descriptive statistics
#' 
#' This function returns descriptive statistics for all levels of a factor vector (counts, proportions and confidence intervals) or for a numeric vector (means, standard deviations, confidence intervals).
#' 
#' @export
#' 
#' @importFrom dplyr bind_rows group_by mutate n relocate rename select summarize ungroup
#' @importFrom magrittr %>%
#' @importFrom tidyselect all_of
#' 
#' @param df Required: a data frame containing the `outcome_variable` (and `grouping_variable` if applicable).
#' @param outcome_variable Required: one or more vectors.
#' @param grouping_variable Optional: one or more vectors.
#' 
#' @return 
#' Returns a tibble of descriptive statistics.
#' 
#' @examples
#' 
#' # Get descriptive stats for numeric vectors
#' get_descriptives(
#'   df = mtcars,
#'   outcome_variable = c(cyl, mpg)
#' )
#' 
#' # A tibble: 2 × 6
#' # variable     n  mean    sd ci_lower ci_upper
#' # <chr>    <int> <dbl> <dbl>    <dbl>    <dbl>
#' # 1 cyl         32  6.19  1.79     5.54     6.83
#' # 2 mpg         32 20.1   6.03    17.9     22.3
#' 
#' # Get descriptive stats for non-numeric vector
#' get_descriptives(
#'   df = mtcars %>%
#'     mutate(cyl = cyl %>% factor),
#'   outcome_variable = cyl
#' )
#' 
#' # A tibble: 3 × 6
#' # variable level     n  prop ci_lower ci_upper
#' # <chr>    <fct> <int> <dbl>    <dbl>    <dbl>
#' # 1 cyl      4        11  34.4    17.9      50.8
#' # 2 cyl      6         7  21.9     7.55     36.2
#' # 3 cyl      8        14  43.8    26.6      60.9
#' 
#' # Get descriptive stats for numeric vectors by groups
#' get_descriptives(
#'   df = mtcars %>%
#'     mutate(cyl = cyl %>% factor),
#'    outcome_variable = c(mpg, disp),
#'    grouping_variable = cyl
#' )
#' 
#' # A tibble: 6 × 7
#' # variable cyl       n  mean    sd ci_lower ci_upper
#' # <chr>    <fct> <int> <dbl> <dbl>    <dbl>    <dbl>
#' # 1 mpg      4        11  26.7  4.51     23.6     29.7
#' # 2 mpg      6         7  19.7  1.45     18.4     21.1
#' # 3 mpg      8        14  15.1  2.56     13.6     16.6
#' # 4 disp     4        11 105.  26.9      87.1    123. 
#' # 5 disp     6         7 183.  41.6     145.     222. 
#' # 6 disp     8        14 353.  67.8     314.     392.
#' 
#' # Set seed for reproducibility
#' set.seed(123)
#' 
#' # Get descriptive stats for non-numeric vector by group
#' get_descriptives(
#'   df = mtcars %>%
#'     mutate(
#'       cyl = cyl %>% factor,
#'       group = sample(
#'         x = c("Group 1", "Group 2"),
#'         size = nrow(mtcars),
#'         replace = TRUE,
#'         prob = c(0.35, 1 - 0.35)
#'       )
#'     ),
#'   outcome_variable = cyl,
#'   grouping_variable = group
#' )
#' 
#' # A tibble: 2 × 7
#' # variable group       n  mean    sd ci_lower ci_upper
#' # <chr>    <chr>   <int> <dbl> <dbl>    <dbl>    <dbl>
#' # 1 cyl      Group 1    15  6.27  1.83     5.25     7.28
#' # 2 cyl      Group 2    17  6.12  1.80     5.19     7.04


get_descriptives <- function(df, outcome_variable, grouping_variable) {
  
  # Convert to string
  outcome_variable <- df %>%
    dplyr::select({{outcome_variable}}) %>%
    names
  
  # Convert to string
  grouping_variable <- df %>%
    dplyr::select({{grouping_variable}}) %>%
    names
  
  # Iterate outcome_variable
  tab <- dplyr::bind_rows(
    lapply(
      X = outcome_variable,
      FUN = function(y) {
        
        # If outcome_variable non-numeric
        if(df %>% dplyr::select(tidyselect::all_of(y)) %>% unlist %>% class %in% c("character", "factor")) {
          
          # Get counts, props and CIs
          tab <- df %>%
            { if(length(grouping_variable)) dplyr::group_by(., .data[[y]], .data[[grouping_variable]]) else dplyr::group_by(., .data[[y]]) } %>%
            dplyr::summarize(n = dplyr::n()) %>%
            dplyr::mutate(
              prop = n / sum(n),
              ci_lower = (prop - qnorm(1 - 0.05 / 2) * sqrt(prop * (1 - prop) / sum(n))) * 100,
              ci_upper = (prop + qnorm(1 - 0.05 / 2) * sqrt(prop * (1 - prop) / sum(n))) * 100,
              prop = prop * 100
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(variable = df %>% dplyr::select(tidyselect::all_of(y)) %>% names) %>%
            dplyr::rename(level = !! y) %>%
            dplyr::relocate(variable, .before = level)
          
          return(tab)
        
        # Else, if outcome_variable numeric
        } else {
          
          # Get means, SDs and CIs
          tab <- { if(length(grouping_variable)) df %>% dplyr::group_by(.data[[grouping_variable]]) else df } %>%
            dplyr::summarize(
              n = dplyr::n(),
              mean = mean(.data[[y]], na.rm = TRUE),
              sd = sd(.data[[y]], na.rm = TRUE),
              ci_lower = mean - qt(0.975, df = n - 1) * sd / sqrt(n),
              ci_upper = mean + qt(0.975, df = n - 1) * sd / sqrt(n)
            ) %>%
            dplyr::ungroup() %>%
            dplyr::mutate(variable = df %>% select(tidyselect::all_of(y)) %>% names) %>%
            dplyr::relocate(variable)
          
          return(tab)
          
        }
        
      }
    )
  )
  
  return(tab %>% as_tibble)
  
}