#' Render bar plot
#'
#' This function renders a bar plot.
#'
#' @export
#'
#' @importFrom dplyr across intersect mutate rename sym
#' @importFrom ggplot2 aes element_blank element_line element_text geom_bar geom_errorbar geom_text ggplot labs margin position_dodge scale_fill_manual theme theme_minimal
#' @importFrom ggtext element_markdown
#' @importFrom magrittr %>%
#' @importFrom stats na.omit
#' @importFrom stringr str_wrap
#' @importFrom viridis viridis
#'
#' @param df Required: a data frame returned from [anyhoodle::get_descriptives()].
#' @param plot_width Optional: a length-one numeric vector (default: `NULL`).
#' @param plot_height Optional: a length-one numeric vector (default: `NULL`).
#' @param plot_font_face Optional: a length-one character vector (default: "Roboto").
#' @param plot_title Optional: a length-one character vector (default: "").
#' @param plot_title_font_size Optional: a length-one numeric vector (default: 10).
#' @param legend_position Optional: a length-one character vector (default: "bottom"; available values: "top", "right", "bottom", "left", "none").
#' @param legend_font_size Optional: a length-one numeric vector (default: 10).
#' @param bar_transparency Optional: a length-one numeric vector (default: 0.7).
#' @param color_palette Optional: a character vector (default: `viridis::viridis(15)`).
#' @param show_grid_lines Optional: a length-one logical vector (default: `FALSE`).
#' @param show_axis_lines Optional: a length-one logical vector (default: `TRUE`).
#' @param show_x_axis Optional: a length-one logical vector (default: `TRUE`).
#' @param show_y_axis Optional: a length-one logical vector (default: `FALSE`).
#' @param x_axis_label Optional: a length-one character vector (default: "").
#' @param y_axis_label Optional: a length-one character vector (default: "").
#' @param y_axis_breaks Optional: a length-one numeric vector (default: 5).
#' @param axis_text_font_size Optional: a length-one numeric vector (default: 10).
#' @param label_font_size Optional: a length-one numeric vector (default: 2.5).
#' @param label_wrap_size Optional: a length-one numeric vector (default: 20).
#' @param label_color Optional: a length-one character vector (default: "#434343").
#' @param item_option_wrap_size Optional: a length-one numeric vector (default: 18).
#' @param error_bar_size Optional: a length-one numeric vector (default: 0.05).
#' @param data_label_position Optional: a length-one numeric vector (default: 2).
#' @param decimals Optional: a length-one numeric vector (default: 1).
#' @param show_n Optional: a length-one logical vector (default: `FALSE`).
#'
#' @return
#' Returns a bar plot.
#'
#' @examples
#'
#' # Render bar plot of means for multiple numeric vectors
#' # get_descriptives(
#' #   df = mtcars,
#' #   outcome_variable = c(mpg, disp, hp)
#' # ) %>%
#' # render_bar_plot
#'
#' # Render bar plot of means for multiple numeric vectors by a grouping variable
#' # get_descriptives(
#' #   df = mtcars %>%
#' #     mutate(cyl = factor(cyl)),
#' #   outcome_variable = c(mpg, disp),
#' #   grouping_variable = cyl
#' # ) %>%
#' # render_bar_plot
#'
#' # Render bar plot of props for a non-numeric vector
#' # get_descriptives(
#' #   df = mtcars %>%
#' #     mutate(cyl = factor(cyl)),
#' #   outcome_variable = cyl
#' # ) %>%
#' # render_bar_plot
#'
#' # Render bar plot of props for multiple non-numeric vectors
#' # get_descriptives(
#' #   df = mtcars %>%
#' #     mutate(
#' #       cyl = factor(cyl),
#' #       cyl2 = cyl
#' #     ),
#' #   outcome_variable = c(cyl, cyl2)
#' # ) %>%
#' # render_bar_plot

render_bar_plot <- function(
    df,
    plot_width = NULL,
    plot_height = NULL,
    plot_font_face = "Roboto",
    plot_title = "",
    plot_title_font_size = 10,
    legend_position = c("top", "right", "bottom", "left", "none")[3],
    legend_font_size = 10,
    bar_transparency = 0.7,
    color_palette = viridis::viridis(15),
    show_grid_lines = FALSE,
    show_axis_lines = TRUE,
    show_x_axis = TRUE,
    show_y_axis = FALSE,
    x_axis_label = "",
    y_axis_label = "",
    y_axis_breaks = 5,
    axis_text_font_size = 10,
    label_font_size = 2.5,
    label_wrap_size = 20,
    label_color = "#434343",
    item_option_wrap_size = 18,
    error_bar_size = 0.05,
    data_label_position = 0.5,
    decimals = 1,
    show_n = FALSE
) {

  # Check whether optional arguments are set and update arguments based on user selections
  if(is.numeric(plot_width) & is.numeric(plot_height)) options(repr.plot.width = plot_width, repr.plot.height = plot_height)
  if(isTRUE(show_grid_lines)) show_grid_lines <- element_line() else show_grid_lines <- element_blank()
  if(isTRUE(show_axis_lines)) show_axis_lines <- element_line(color = "#434343") else show_axis_lines <- element_blank()
  if(isFALSE(show_x_axis)) x_axis_text <- element_blank() else x_axis_text <- element_text(color = label_color, size = axis_text_font_size)
  if(isFALSE(show_y_axis)) y_axis_text <- element_blank() else y_axis_text <- element_text(color = label_color, size = axis_text_font_size)
  if(is.numeric(data_label_position)) data_label_position <- data_label_position * -1

  # If prop vector exists, rename as "mean"
  if("prop" %in% names(df)) {

    is_prop <- TRUE
    df <- df %>% dplyr::rename(mean = prop)
    max_y_value <- as.integer(max(df$mean) + 20)

  } else {

    is_prop <- FALSE

    if(x_axis_label != "") x_axis_text <- element_blank()
    max_y_value <- max(df$mean) + min(df$sd)
    max_y_value <- as.integer(max(df$mean) + max(df$mean) * 0.2)

  }

  # Round mean vector to one decimal
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        .cols = mean,
        .fns = ~ round(.x, decimals)
      )
    )

  # Wrap vector names if too long
  df <- df %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::intersect(names(.), c("variable", "level")),
        .fns = ~ stringr::str_wrap(
          string = .x,
          width = item_option_wrap_size
        )
      )
    )

  # Define required variables for plotting
  variables <- length(unique(stats::na.omit(df$variable)))
  levels <- length(unique(stats::na.omit(df$level)))
  grouping_variable <- df %>%
    dplyr::select(level:n) %>%
    dplyr::select(-level, -n) %>%
    names
  grouping_variables <- df %>%
    dplyr::select(level:n) %>%
    dplyr::select(-level, -n) %>%
    names %>%
    length

  if(levels > 0) {

    # Wrap vector names if too long
    df$level <- gsub(
      pattern = "\\s+$",
      replace = "",
      x = gsub(
        pattern = paste0("(.{", label_wrap_size, "})"),
        replacement = "\\1\n",
        x = df$level
      )
    )

  }

  # Select which variables are passed to ggplot2's mapping
  if(levels < 2) {

    if(grouping_variables > 0) {

      denominator <- df %>% dplyr::select(!! grouping_variable) %>%
        stats::na.omit() %>%
        unique %>%
        unlist %>%
        unname %>%
        length
      x_variable <- "variable"
      fill_variable <- grouping_variable

    } else {

      denominator <- variables
      x_variable <- fill_variable <- "variable"

    }

  } else {

    if(variables > 1) {

      denominator <- levels
      x_variable <- "variable"
      fill_variable <- "level"

    } else {

      if(grouping_variables > 0) {

        denominator <- max(levels, length(levels(df[[grouping_variable]])))
        x_variable <- "level"
        fill_variable <- grouping_variable

      } else {

        denominator <- levels
        x_variable <- fill_variable <- "level"

      }

    }

  }

  # Select the number of colors needed
  if(isTRUE(all.equal(color_palette, viridis::viridis(15)))) {

    fill_colors <- color_palette[
      seq(
        from = 1,
        to = length(color_palette),
        by = floor(length(color_palette) / denominator)
      )[1:denominator]
    ]

  } else {

    fill_colors <- color_palette

  }

  if(grouping_variables > 0) {

    # Ensure grouping variables are factors
    df <- df %>%
      dplyr::mutate(
        dplyr::across(
          .cols = !! dplyr::sym(grouping_variable),
          .fns = ~ factor(.x)
        )
      )

  }

  if(isTRUE(show_n)) {

    labels <- paste0(
      "\n(",
      trimws(
        format(
          x = df$n,
          big.mark = ","
        )
      ),
      ")"
    )

  } else {

    labels <- rep("", nrow(df))

  }

  # Set mapping content
  mapping_content <- aes(
    x = !! dplyr::sym(x_variable),
    y = mean,
    fill = !! dplyr::sym(fill_variable)
  )

  # Render bar plot
  ggplot2::ggplot(
    data = df,
    mapping = mapping_content
  ) +
    ggplot2::geom_bar(
      stat = "identity",
      width = 0.8,
      position = ggplot2::position_dodge(width = 0.9),
      alpha = bar_transparency
    ) +
    ggplot2::geom_text(
      position = ggplot2::position_dodge(width = 0.9),
      ggplot2::aes(
        label = paste0(
          format(
            x = df$mean,
            nsmall = decimals
          ),
          ifelse(
            test = isTRUE(is_prop),
            yes = paste0("%"),
            no = paste0("")
          ),
          labels
        ),
        vjust = data_label_position,
        y = ci_upper
      ),
      size = label_font_size
    ) +
    ggplot2::geom_errorbar(
      ggplot2::aes(ymin = pmax(0, round(ci_lower, 2)), ymax = round(ci_upper, 2)),
      color = "#434343",
      width = error_bar_size,
      position = ggplot2::position_dodge(width = 0.9)
    ) +
    ggplot2::scale_y_continuous(limits = c(0, max_y_value)) +
    ggplot2::scale_fill_manual(values = fill_colors) +
    ggplot2::labs(
      title = plot_title,
      x = x_axis_label,
      y = y_axis_label
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      text = ggplot2::element_text(family = plot_font_face),
      plot.title = ggplot2::element_text(size = plot_title_font_size, margin = ggplot2::margin(0, 0, plot_title_font_size * 2, 0)),
      axis.title.x = ggplot2::element_text(color = label_color, margin = ggplot2::margin(0, axis_text_font_size * 1.25, 0)),
      axis.title.y = ggplot2::element_text(size = axis_text_font_size, color = label_color, margin = ggplot2::margin(0, axis_text_font_size * 0.75, 0)),
      axis.text.x = x_axis_text,
      axis.text.y = y_axis_text,
      legend.position = legend_position,
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = legend_font_size, color = label_color),
      plot.caption = ggtext::element_markdown(size = label_font_size + 6, color = label_color, hjust = 0),
      panel.grid.major = show_grid_lines,
      panel.grid.minor = show_grid_lines,
      axis.line = show_axis_lines
    )

}
