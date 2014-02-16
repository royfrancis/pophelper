\name{getDim}
\alias{getDim}
\title{Internal: Get dimensions for figures.}
\usage{
  getDim(ind, units = "cm", height = NA, width = NA,
    res = 300, plotnum = 1)
}
\arguments{
  \item{ind}{Number of individuals.}

  \item{units}{unit of dimension: "cm","mm","in".}

  \item{height}{Height of each plot.}

  \item{width}{Width of each plot.}

  \item{res}{Resolution of the figure.}

  \item{plotnum}{Number of plot in the figure.}
}
\value{
  a vector with height and width.
}
\description{
  Generate height and width of figure based on number of
  individuals. This is an internal function that calculates
  figure dimensions for export.
}

