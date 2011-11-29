\name{view}
\alias{view}
\alias{view,ANY-method}
\alias{view,ViewerData-method}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
View a Data Set
}
\description{
  Create a graphics device containing a view of a data set,
  plus a tcltk window to interactively modify the view.
}
\usage{
view(datasrc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datasrc}{
    A vector or data frame or object derived from the \code{ViewerData} class.
  }
}
\section{Methods}{
  \describe{
    \item{view}{\code{signature(datasrc = "ANY")}:
      Attempts to create appropriate \code{ViewerData} object to view. }
    \item{view}{\code{signature(datasrc = "ViewerData")}:
      Generates view of object. }
  }
}
\value{
  Called for its side-effect.
}
\author{
 Paul Murrell
}
\seealso{
\code{\link{viewerData}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ dynamic }
