\name{lrshrink}
\Rdversion{1.1}
\alias{lrshrink}
\alias{lrgrow}
\alias{udshrink}
\alias{udgrow}
\title{
  Zoom a data set view  
}
\description{
  These functions modify the view of a data set by allowing
  different sorts of zooming.  Horizontal zoom always adds
  or removes a whole number of columns to the view.
  Vertical zooming adds or removes either a number of rows
  or a number of pages to the view.
}
\usage{
lrshrink(v, side = "right", n = 1)
lrgrow(v, side = "right", n = 1)
udshrink(v, side = "top", page = FALSE, n = 1)
udgrow(v, side = "top", page = FALSE, n = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{v}{
    An object of class \code{"Viewer"}. 
  }
  \item{side}{
    Which direction to zoom in (left, right, up, or down).
  }
  \item{page}{
    Logical indicating whether vertical zoom is a multiple
    of a number of rows or a number of pages.
  }
  \item{n}{
    How many rows or columns to zoom.
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
