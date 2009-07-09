\name{ViewerDataVector-class}
\Rdversion{1.1}
\docType{class}
\alias{ViewerData-class}
\alias{ViewerDataVector-class}
\alias{colNames,ViewerDataVector,numeric-method}
\alias{colWidths,ViewerDataVector-method}
\alias{dimensions,ViewerDataVector-method}
\alias{getText,ViewerDataVector,numeric,numeric-method}

\title{Class "ViewerDataVector" }
\description{
  An object representing the (atomic) data to view.
}
\section{Objects from the Class}{
  Objects can be created using the \code{viewerData} function.
}
\section{Slots}{
  \describe{
    \item{\code{x}:}{Object of class \code{"vector"} giving the data to view. }
    \item{\code{name}:}{Object of class \code{"character"} giving a
      label for the data. }
    \item{\code{width}:}{Object of class \code{"numeric"} giving the
      number of characters in a text version of the data.}
  }
}
\section{Extends}{
The virtual class \code{"\linkS4class{ViewerData}"}, directly.
}
\section{Methods}{
  \describe{
    \item{colNames}{\code{signature(data = "ViewerDataVector", cols =
        "numeric")}:
      provide the names of the specified columns (just the data name). }
    \item{colWidths}{\code{signature(data = "ViewerDataVector")}:
      provide the number of characters in each of the specified columns
      (just the width of the data).}
    \item{dimensions}{\code{signature(data = "ViewerDataVector")}:
      provide the number of rows and columns in the data (just the rows).}
    \item{getText}{\code{signature(data = "ViewerDataVector", rows =
        "numeric", cols = "numeric")}:
      provide a text version of a subset of the data as specified by the
      rows and columns.}
  }
}
\author{Paul Murrell}
\seealso{
  \code{\linkS4class{Viewer}}
}
\examples{
showClass("ViewerDataVector")
}
\keyword{classes}
