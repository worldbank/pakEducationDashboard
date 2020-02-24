#' plot_lines_weighted
#' Common ggplot layers for the line plots using weighted data
#' 
#' @param data data.frame: The dataset to be plotted
#' @param x object: Variable to be mapped to x aesthetic
#' @param y object: Variable to be mapped to y aesthetic
#' @param color object: Variable to be mapped to color aesthetic
#' @param dataset object: Variable to be displayed in tooltip
#' @param gender object: Variable to be displayed in tooltip
#' @param year object: Variable to be displayed in tooltip
#' @param tooltip_value object: Variable to be displayed in tooltip
#'
#' @return
#' @export
#'
plot_lines_weighted <- function(data,
                                x,
                                y,
                                color,
                                dataset,
                                gender,
                                year,
                                tooltip_value,
                                font_size = 20,
                                point_size = 6,
                                line_size = 1.8
                                ) 
  {
  
  # Adjust scale according to indicator
  if (stringr::str_detect(unique(data$indicator), "^egra")) {
    y_scale <- ggplot2::scale_y_continuous(limits = c(0, 100))
  } else {
    y_scale <- ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent)
  }
  
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = {{x}}, 
                                          y = {{y}}, 
                                          color = {{color}},
                                          tooltip = paste("Value:", {{tooltip_value}},
                                                          "<br />Year:", {{year}},
                                                          "<br />Dataset:", {{dataset}},
                                                          "<br />Gender:", {{gender}}))) +
    ggplot2::geom_line(ggplot2::aes(group = {{gender}}),
                       size = ggplot2::rel(line_size)) +
    ggiraph::geom_point_interactive(size = ggplot2::rel(point_size)) +
    y_scale +
    ggplot2::scale_x_continuous(breaks = integer_breaks()) +
    ggthemes::scale_color_colorblind() +
    cowplot::theme_cowplot(font_size) +
    ggplot2::theme(
      legend.title    = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::labs(
      x = "",
      y = ""
    ) 
  
  return(p)
}

#' plot_lines
#' Common ggplot layers for the line plots using non-weighted data
#' 
#' @param data data.frame: The dataset to be plotted
#' @param x object: Variable to be mapped to x aesthetic
#' @param y object: Variable to be mapped to y aesthetic
#' @param color object: Variable to be mapped to color aesthetic
#' @param dataset object: Variable to be displayed in tooltip
#' @param gender object: Variable to be displayed in tooltip
#' @param year object: Variable to be displayed in tooltip
#' @param tooltip_value object: Variable to be displayed in tooltip
#'
#' @return
#' @export
#'
plot_lines <- function(data,
                       x,
                       y,
                       color,
                       dataset,
                       gender,
                       year,
                       tooltip_value,
                       font_size = 20,
                       point_size = 5,
                       line_size = 1.6
) 
{
  
  # Adjust scale according to indicator
  if (stringr::str_detect(unique(data$indicator), "^egra")) {
    y_scale <- ggplot2::scale_y_continuous(limits = c(0, 100))
  } else {
    y_scale <- ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent)
  }
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = {{x}}, 
                                          y = {{y}}, 
                                          color = {{color}},
                                          tooltip = paste("Value:", {{tooltip_value}},
                                                          "<br />Year:", {{year}},
                                                          "<br />Dataset:", {{dataset}},
                                                          "<br />Gender:", {{gender}}))) +
    ggplot2::geom_line(ggplot2::aes(group = interaction({{dataset}}, {{gender}}), 
                                    linetype = {{dataset}}),
                       size = ggplot2::rel(line_size),
                       alpha = .6 ) +
    ggiraph::geom_point_interactive(ggplot2::aes(shape = {{dataset}}),
                                    size = ggplot2::rel(point_size), alpha = .6) +
    y_scale +
    ggplot2::scale_x_continuous(breaks = integer_breaks()) +
    ggthemes::scale_color_colorblind() +
    cowplot::theme_cowplot(font_size) +
    ggplot2::theme(
      legend.title    = ggplot2::element_blank(),
      legend.position = "none"
    ) +
    ggplot2::labs(
      x = "",
      y = ""
    ) 
  
  return(p)
}

#' plot_map
#' Common ggplot layers for the maps
#' 
#' @param data data.frame: The dataset to be plotted
#' @param fill object: Variable to be mapped to fill aesthetic
#' @param data_id object: Variable to be mapped to data_id
#' @param year object: Variable to be displayed in tooltip
#' @param tooltip_region_header object: Variable to be displayed in tooltip (region header)
#' @param tooltip_region_value object: Variable to be displayed in tooltip (region)
#' @param tooltip_value object: Variable to be displayed in tooltip (statistic value)
#' @param font_size numeric: Set the chart's font size value
#'
#' @return
#' @export
#'
plot_map <- function(data,
                     fill,
                     data_id,
                     year,
                     tooltip_region_header,
                     tooltip_region_value,
                     tooltip_value,
                     font_size = 20
) 
{
  
  
  # Adjust scale according to indicator
  if (stringr::str_detect(unique(data$indicator), "^egra")) {
    fill_scale <- ggplot2::scale_fill_viridis_c(limits = c(0, 100), guide = FALSE)
  } else {
    fill_scale <- ggplot2::scale_fill_viridis_c(limits = c(0, 1), guide = FALSE)
  }
  
  p <- ggplot2::ggplot(data) +
    ggiraph::geom_sf_interactive(ggplot2::aes(fill = {{fill}},
                                              tooltip = paste(tooltip_region_header, {{tooltip_region_value}},
                                                              "<br />Value:", {{tooltip_value}},
                                                              "<br />Year:", {{year}}),
                                              data_id = {{data_id}}
    )) +
    fill_scale +
    ggthemes::theme_map(base_size = font_size) +
    ggplot2::labs(
      fill = ""
    )
  
  
  return(p)
}

# A function factory for getting integer y-axis values.
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}