\name{simpleViewer}
\Rdversion{1.1}
\alias{simpleViewer}
\title{
  A view a data source.
}
\description{
  Creates an object representing a view of a data source.
}
\usage{
simpleViewer(data, state = viewerState(), dev = viewerDevice())
}
\arguments{
  \item{data}{
    The data source being viewed (an object of class
    \code{\linkS4class{ViewerData}})
  }
  \item{state}{
    Information on the current view (an object of class
    \code{\linkS4class{ViewerState}})
  }
  \item{dev}{
    The device to view the data on  (an object of class
    \code{\linkS4class{ViewerDevice}}
}
}
\details{
  Nothing is drawn when this object is created.  You need to call
  the \code{draw} function to produce anything.  Several other functions
  can be used to modify the view, e.g., \code{lrscroll} and
  \code{udscroll}, then \code{draw} can be called again to
  see the modified view.
}
\value{
  An object of class \code{"\linkS4class{ViewerSimple}"}.
}
\author{Paul Murrell}
\seealso{
  \code{\linkS4class{ViewerSimple}}
  and
  \code{\link{viewerData}}
}
\examples{
}
\keyword{ dynamic }
