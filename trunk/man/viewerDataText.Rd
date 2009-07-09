\name{viewerDataText}
\Rdversion{1.1}
\alias{viewerDataText}
\title{
  Prepare a text file for viewing.
}
\description{
  This function derives various important attributes of a
  text file and stores them so that viewing the file
  can occur efficiently.
}
\usage{
viewerDataText(filename, width = NULL, estimate = NULL, index = FALSE, ...)
}
\arguments{
  \item{filename}{
    Name of the file.
  }
  \item{width}{
    The number of characters on each line to view.  
  }
  \item{estimate}{
    A number of lines of the file to be read to estimate
    the maximum line length.
  }
  \item{index}{
    A logical value indicating whether to generate a byte
    index to the start of each line in the file.
  }
  \item{\dots}{
    Arguments passed to the function(s) used to read the file
    (not currently used).
  }
}
\details{
  If both \code{width} and \code{estimate} are \code{NULL}
  then the maximum line width is determined by reading the entire file.

  For large files, setting \code{index=TRUE} will vastly speed
  up the initialisation and viewing of the file.
}
\value{
  An object of class \code{"\linkS4class{ViewerDataText}"}.
}
\author{Paul Murrell}
\seealso{
  \code{\linkS4class{ViewerDataText}}
  and
  \code{\link{viewerData}}
}
\examples{
}
\keyword{ dynamic }
