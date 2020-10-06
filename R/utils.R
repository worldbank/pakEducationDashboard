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
                       line_size = 1.6,
                       title,
                       subtitle
) 
{
  subtitle <- stringr::str_wrap(subtitle, width = 160)
  
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
  
  
  if (!is.null(wght_data)) {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = {{x}}, 
                                            y = {{y}}, 
                                            color = {{color}},
                                            tooltip = paste("Value:", {{tooltip_value}},
                                                            "<br />Year:", {{year}},
                                                            "<br />Dataset:", {{dataset}},
                                                            "<br />Group:", {{group}})))  +
      ggplot2::geom_line(ggplot2::aes(group = interaction({{dataset}}, {{group}}), 
                                      linetype = {{dataset}}),
                         color = "grey",
                         size = ggplot2::rel(line_size),
                         alpha = .6 ) +
      ggiraph::geom_point_interactive(size = ggplot2::rel(point_size), 
                                      color = "grey",
                                      alpha = .6)
  } else {
    p <- ggplot2::ggplot(data, ggplot2::aes(x = {{x}}, 
                                            y = {{y}}, 
                                            color = {{color}},
                                            alpha = as.character(sample_size_cat),
                                            tooltip = paste("Value:", {{tooltip_value}},
                                                            "<br />Year:", {{year}},
                                                            "<br />Dataset:", {{dataset}},
                                                            "<br />Group:", {{group}},
                                                            "<br />Sample size:", sample_size))) +
      ggplot2::geom_line(ggplot2::aes(group = interaction({{dataset}}, {{group}}), 
                                      linetype = {{dataset}}),
                         size = ggplot2::rel(line_size)) +
      ggiraph::geom_point_interactive(size = ggplot2::rel(point_size))
  }
  
  p <- p +
    ggplot2::scale_shape_manual(values = shps) +
    ggplot2::scale_alpha_manual(values = c("0" = 0.1, "1" = 1)) +
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
      title = title,
      subtitle = subtitle,
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
                     font_size = 20,
                     title,
                     subtitle
) 
{
  subtitle <- stringr::str_wrap(subtitle, width = 90)
  
  sf::st_crs(dashed_border) <- 4326
  sf::st_crs(dotted_border) <- 4326
  sf::st_crs(plain_border)  <- 4326
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
      title = title,
      subtitle = subtitle,
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

#' create_footer
#' From https://github.com/bbc/bbplot/blob/master/R/finalise_plot.R
#'
#' @param source_name character: data source
#' @param logo_image_path 
#'
#' @return
#' @export
#'
create_footer <- function (source_name, logo_image_path) {
  #Make the footer
  footer <- grid::grobTree(#grid::linesGrob(x = grid::unit(c(0, 1), "npc"), y = grid::unit(1.1, "npc")),
                           grid::textGrob(source_name,
                                          x = 0.004, hjust = 0, gp = grid::gpar(fontsize=16)),
                           grid::rasterGrob(png::readPNG(logo_image_path), x = 0.944))
  return(footer)
  
}

#' left_align
#' From https://github.com/bbc/bbplot/blob/master/R/finalise_plot.R
#'
#' @param plot_name ggplot: Plot name
#' @param pieces character: Plot elements to be left aligned
#'
#' @return grob
#' @export
#'
left_align <- function(plot_name, pieces){
  grob <- ggplot2::ggplotGrob(plot_name)
  n <- length(pieces)
  grob$layout$l[grob$layout$name %in% pieces] <- 2
  return(grob)
}

#' Arrange alignment and add logo
#' From https://github.com/bbc/bbplot/blob/master/R/finalise_plot.R
#' It will left align your title, subtitle and source, add the logo at the bottom right
#' @param plot_name The variable name of the plot you have created that you want to format
#' @param source_name The text you want to come after the text 'Source:' in the bottom left hand side of your side
#' @param width_pixels Width in pixels that you want to save your chart to - defaults to 640
#' @param height_pixels Height in pixels that you want to save your chart to - defaults to 450
#' @param logo_image_path File path for the logo image you want to use in the right hand side of your chart,
#'  which needs to be a PNG file - defaults to BBC blocks image that sits within the data folder of your package
#' @return (invisibly) an updated ggplot object.

#' @examples
#' finalise_plot(plot_name = myplot,
#' source = "The source for my data",
#' width_pixels = 640,
#' height_pixels = 450,
#' logo_image_path = "logo_image_filepath.png"
#' )
#'
#' @export
finalise_plot <- function(plot_name,
                          source_name = "Source = World Bank",
                          width_pixels=640,
                          height_pixels=450,
                          logo_image_path = system.file("app/www", "logo.png", package = "pakEducationDashboard")) {
  
  footer <- create_footer(source_name, logo_image_path)
  
  #Draw your left-aligned grid
  plot_left_aligned <- left_align(plot_name, c("subtitle", "title", "caption"))
  plot_grid <- ggpubr::ggarrange(plot_left_aligned, footer,
                                 ncol = 1, nrow = 2,
                                 heights = c(1, 0.045/(height_pixels/450)))
  
  ## Return (invisibly) a copy of the graph. Can be assigned to a
  ## variable or silently ignored.
  invisible(plot_grid)
}

#' Add WB theme to ggplot chart
#'
#' @export

theme_wb <- function() {
  font <- "calibri"
  
  ggplot2::theme(
    
    #Text format:
    #This sets the font, size, type and colour of text for the chart's title
    plot.title = ggplot2::element_text(family = font,
                                       face = "bold"),
    #This sets the font, size, type and colour of text for the chart's subtitle, as well as setting a margin between the title and the subtitle
    plot.subtitle = ggplot2::element_text(family = font,
                                          margin = ggplot2::margin(9, 0, 9, 0)),
    plot.caption = ggplot2::element_blank()
    #This leaves the caption text element empty, because it is set elsewhere in the finalise plot function
  )
}