% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{plot_map}
\alias{plot_map}
\title{plot_map
Common ggplot layers for the maps}
\usage{
plot_map(
  data,
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
}
\arguments{
\item{data}{data.frame: The dataset to be plotted}

\item{fill}{object: Variable to be mapped to fill aesthetic}

\item{data_id}{object: Variable to be mapped to data_id}

\item{year}{object: Variable to be displayed in tooltip}

\item{tooltip_region_header}{object: Variable to be displayed in tooltip (region header)}

\item{tooltip_region_value}{object: Variable to be displayed in tooltip (region)}

\item{tooltip_value}{object: Variable to be displayed in tooltip (statistic value)}

\item{tooltip_dataset}{object: Variable to be displayed in tooltip}

\item{font_size}{numeric: Set the chart's font size value}
}
\value{

}
\description{
plot_map
Common ggplot layers for the maps
}
