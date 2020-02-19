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
                                tooltip_value
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
                                          text = paste("Value:", {{tooltip_value}},
                                                       "<br />Year:", {{year}},
                                                       "<br />Dataset:", {{dataset}},
                                                       "<br />Gender:", {{gender}}))) +
    ggplot2::geom_line(ggplot2::aes(group = {{gender}}),
                       size = ggplot2::rel(0.8)) +
    ggplot2::geom_point(size = ggplot2::rel(2.8)) +
    y_scale +
    ggplot2::scale_x_continuous(breaks = integer_breaks()) +
    ggthemes::scale_color_colorblind() +
    cowplot::theme_cowplot(14) +
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
                       tooltip_value
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
                                          text = paste("Value:", {{tooltip_value}},
                                                       "<br />Year:", {{year}},
                                                       "<br />Dataset:", {{dataset}},
                                                       "<br />Gender:", {{gender}}))) +
    ggplot2::geom_line(ggplot2::aes(group = interaction({{dataset}}, {{gender}}), 
                                    linetype = {{dataset}}),
                       size = ggplot2::rel(0.6),
                       alpha = .6 ) +
    ggplot2::geom_point(ggplot2::aes(shape = {{dataset}}),
                        size = ggplot2::rel(2.2), alpha = .6) +
    y_scale +
    ggplot2::scale_x_continuous(breaks = integer_breaks()) +
    ggthemes::scale_color_colorblind() +
    cowplot::theme_cowplot(14) +
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


# A function factory for getting integer y-axis values.
integer_breaks <- function(n = 5, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n, ...))
    names(breaks) <- attr(breaks, "labels")
    breaks
  }
  return(fxn)
}