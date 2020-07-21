#' plot_lines_weighted
#' Common ggplot layers for the line plots using weighted data
#' 
#' @param data data.frame: The dataset to be plotted
#' @param x object: Variable to be mapped to x aesthetic
#' @param y object: Variable to be mapped to y aesthetic
#' @param color object: Variable to be mapped to color aesthetic
#' @param dataset object: Variable to be displayed in tooltip
#' @param group object: Variable used for dis-aggregation & to be displayed in tooltip
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
                                group,
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
  
  # Adjust for group
  # scale_values = ifelse(unique(data$group) == "Both", "#F05123", c("#F05123","#97252B"))
  scale_values <- c("#F05123","#97252B", "#29AFDE", "#6DD6C3", "#EBD197")
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = {{x}}, 
                                          y = {{y}}, 
                                          color = {{color}},
                                          tooltip = paste("Value:", {{tooltip_value}},
                                                          "<br />Year:", {{year}},
                                                          "<br />Dataset:", {{dataset}},
                                                          "<br />Group:", {{group}}))) +
    ggplot2::geom_line(ggplot2::aes(group = {{group}}),
                       size = ggplot2::rel(line_size)) +
    ggiraph::geom_point_interactive(size = ggplot2::rel(point_size)) +
    y_scale +
    ggplot2::scale_x_continuous(breaks = integer_breaks()) +
    ggplot2::scale_color_manual(values = scale_values) +
    cowplot::theme_cowplot(font_size) +
    ggplot2::theme(
      legend.title    = ggplot2::element_blank(),
      legend.position = "none",
      panel.background = ggplot2::element_rect(fill = "#ECF0F1")
    ) +
    ggplot2::labs(
      x = "",
      y = ""
    ) 
  
  return(p)
}

#' plot_lines
#' Common ggplot layers for the line plots
#' 
#' @param data data.frame: The dataset to be plotted
#' @param wght_data data.frame: The dataset containing weighted-mix data
#' @param x object: Variable to be mapped to x aesthetic
#' @param y object: Variable to be mapped to y aesthetic
#' @param color object: Variable to be mapped to color aesthetic
#' @param dataset object: Variable to be displayed in tooltip
#' @param group object: Variable used for dis-aggregation & to be displayed in tooltip
#' @param year object: Variable to be displayed in tooltip
#' @param tooltip_value object: Variable to be displayed in tooltip
#'
#' @return
#' @export
#'
plot_lines <- function(data,
                       wght_data = NULL,
                       x,
                       y,
                       color,
                       dataset,
                       group,
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
  
  # Adjust for group
  # scale_values = ifelse(unique(data$group) == "Both", "#F05123", c("#F05123","#97252B"))
  scale_values <- c("#F05123","#97252B", "#29AFDE", "#6DD6C3", "#EBD197")
  
  # TO account for ggiraph bug
  ## https://github.com/davidgohel/ggiraph/issues/31
  shps <- setNames( c(0, 1, 2, 5, 6, 15, 16), c("Weighted mix","aser", "hies", "pslm",
                                                "mics", "dhs", "egra"))
  
  p <- ggplot2::ggplot(data, ggplot2::aes(x = {{x}}, 
                                          y = {{y}}, 
                                          color = {{color}},
                                          tooltip = paste("Value:", {{tooltip_value}},
                                                          "<br />Year:", {{year}},
                                                          "<br />Dataset:", {{dataset}},
                                                          "<br />Group:", {{group}}))) 
  
  
  if (!is.null(wght_data)) {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(group = interaction({{dataset}}, {{group}}), 
                                      linetype = {{dataset}}),
                         color = "grey",
                         size = ggplot2::rel(line_size),
                         alpha = .6 ) +
      ggiraph::geom_point_interactive(size = ggplot2::rel(point_size), 
                                      color = "grey",
                                      alpha = .6)
  } else {
    p <- p +
      ggplot2::geom_line(ggplot2::aes(group = interaction({{dataset}}, {{group}}), 
                                      linetype = {{dataset}}),
                         size = ggplot2::rel(line_size),
                         alpha = 1 ) +
      ggiraph::geom_point_interactive(size = ggplot2::rel(point_size), alpha = 1)
  }
  
  p <- p +
    ggplot2::scale_shape_manual(values = shps) +
    y_scale +
    ggplot2::scale_x_continuous(breaks = integer_breaks()) +
    ggplot2::scale_color_manual(values = scale_values) +
    cowplot::theme_cowplot(font_size) +
    ggplot2::theme(
      legend.title    = ggplot2::element_blank(),
      legend.position = "none",
      panel.background = ggplot2::element_rect(fill = "#ECF0F1")
    ) +
    ggplot2::labs(
      x = "",
      y = ""
    ) 
  
  if (!is.null(wght_data)) {
    p <- p +
      ggplot2::geom_line(data = wght_data,
                         ggplot2::aes(group = interaction({{dataset}}, {{group}})),
                         #color = {{color}}),
                         size = ggplot2::rel(line_size),
                         alpha = 1) +
      ggiraph::geom_point_interactive(data = wght_data,
                                      #ggplot2::aes(color = {{color}}),
                                      size = ggplot2::rel(point_size), 
                                      alpha = 1)
    
  }
  
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
#' @param tooltip_dataset object: Variable to be displayed in tooltip
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
                     tooltip_dataset,
                     font_size = 20
) 
{
  
  # Adjust scale according to indicator
  if (stringr::str_detect(unique(data$indicator), "^egra") & !is.na(unique(data$indicator))) {
    #fill_scale <- ggplot2::scale_fill_viridis_c(limits = c(0, 100), guide = FALSE)
    fill_scale <- ggplot2::scale_fill_gradientn(colours = c("#002f54", "#009CA7", "#F05123"),
                                                limits = c(0, 100),
                                                breaks = c(0, 25, 50, 75, 100))
  } else {
    #fill_scale <- ggplot2::scale_fill_viridis_c(limits = c(0, 1), guide = FALSE)
    fill_scale <- ggplot2::scale_fill_gradientn(colours = c("#002f54", "#009CA7", "#F05123"),
                                                limits = c(0, 1),
                                                breaks = c(0, 0.25, 0.50, 0.75, 1),
                                                labels = c("0%", "25%", "50%", "75%", "100%"))
  }
  
  p <- ggplot2::ggplot(data) +
    ggiraph::geom_sf_interactive(ggplot2::aes(fill = {{fill}},
                                              tooltip = paste(tooltip_region_header, {{tooltip_region_value}},
                                                              "<br />Value:", {{tooltip_value}},
                                                              "<br />Year:", {{year}},
                                                              "<br />Dataset:", {{tooltip_dataset}}),
                                              data_id = {{data_id}}),
                                 color = "#4d4e4f") +
    ggplot2::geom_sf(data = dashed_border, linetype = "dashed", color = "white") +
    ggplot2::geom_sf(data = dotted_border, linetype = "dotted", color = "#4d4e4f") +
    ggplot2::geom_sf(data = plain_border, color = "#4d4e4f") +
    fill_scale +
    ggplot2::theme(
      legend.background = ggplot2::element_rect(colour = "transparent", fill = "transparent"),
      legend.box.background = ggplot2::element_rect(colour = "transparent", fill = "transparent")
    ) +
    #ggplot2::annotate("text", x = 32.06, y = 68.99, label = "Top-left") +
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