\name{getColours}
\alias{getColours}
\title{Internal: Get Colours}
\usage{
getColours(k)
}
\arguments{
  \item{k}{Number of colours required}
}
\value{
Returns a vector of k colours in hexadecimal format
}
\description{
Generate colours based on number of K. This is an internal
function to generate colours based on number of
populations.
}
\details{
Colours 1 to 12 are custom unique colours. Colours beyond
15 are generated from colour ramp \code{rich.colors()}.
}

