\name{lrscroll}
\Rdversion{1.1}
\alias{lrscroll}
\alias{udscroll}
\alias{lrhome}
\alias{udhome}
\alias{lrend}
\alias{udend}
\title{
  Navigate within a data set view
}
\description{
  These functions modify the view of a data set by allowing
  different sorts of navigation.  Horizontal navigation is
  always in terms of whole columns.  Vertical navigation is
  in terms of either rows or pages.
}
\usage{
lrscroll(v, side = "right", n = 1)
lrhome(v)
lrend(v)
udscroll(v, side = "top", page = FALSE, n = 1)
udhome(v)
udend(v)
}
\arguments{
  \item{v}{
    An object of class \code{"Viewer"}. 
  }
  \item{side}{
    Which direction to navigate in (left, right, up, or down).
  }
  \item{page}{
    Logical indicating whether vertical navigation is a multiple
    of a number of rows or a number of pages.
  }
  \item{n}{
    How many rows or columns to navigate.
  }
}
\value{
  The modified \code{"Viewer"} object (representing the new view of the data).
}
\author{
  Paul Murrell
}
\seealso{
  \code{\link{ViewerSimple-class}}
}
\keyword{ dynamic }
