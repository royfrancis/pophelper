\name{ellipseCI}
\alias{ellipseCI}
\title{Internal: ellipseCI}
\usage{
ellipseCI(x, y, conf = 0.95, np = 100)
}
\arguments{
  \item{x}{vector 1}

  \item{y}{vector 2}

  \item{conf}{confidence interval}

  \item{np}{number of points}
}
\value{
A dataframe with x and y coordinates of the ellipse. Number
of rows is equal to argument np.
}
\description{
Calculate ellipse for bivariate quantile
}
\details{
Obtained from Claude J (2008) Morphometrics with R,
Springer
}

