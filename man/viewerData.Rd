\name{viewerData}
\Rdversion{1.1}
\alias{viewerData}
\title{
  Create a data object to view.
}
\description{
  Creates a data object for viewing.
}
\usage{
viewerData(x, name = NULL)
}
\arguments{
  \item{x}{
    An R object; typically an atomic vector or a list (see Details).
  }
  \item{name}{
    A name to be used to label the view of the data.
  }
}
\details{
  This function is useful for preparing simple atomic vectors
  for viewing \emph{or} for preparing a complex object
  that has no specific function (such as a general list object).
  In the latter case, the normal printed output of the object
  is used as the basis for viewing.

  There are more appropriate functions for
  preparing data frames, external text files, or relational
  databases for viewing.
}
\author{
  Paul Murrell
}
\seealso{
\code{\link{viewerDataFrame}}, 
\code{\link{viewerDataText}}, 
\code{\link{viewerDataMySQL}}.
}
\examples{
}
\keyword{ dynamic }
