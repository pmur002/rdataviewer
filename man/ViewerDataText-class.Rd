\name{ViewerDataText-class}
\Rdversion{1.1}
\docType{class}
\alias{ViewerDataText-class}
\alias{colNames,ViewerDataText,ANY-method}
\alias{colWidths,ViewerDataText-method}
\alias{dimensions,ViewerDataText-method}
\alias{getText,ViewerDataText,ANY,ANY-method}

\title{Class "ViewerDataText" }
\description{
  An object representing the text file to view.
}
\section{Objects from the Class}{
  Objects can be created using the \code{viewerDataText} function.
}
\section{Slots}{
  \describe{
    \item{\code{filename}:}{Object of class \code{"character"} giving
      the name of the text file.}
    \item{\code{width}:}{Object of class \code{"numeric"} giving
      the maximum number of characters on the longest line in the file.}
    \item{\code{nlines}:}{Object of class \code{"numeric"}
      giving the number of lines in the file.}
    \item{\code{readArgs}:}{Object of class \code{"list"}
      giving any additional arguments used to read the file
      (currently not used).}
    \item{\code{index}:}{Object of class \code{"numeric"}
      giving a byte index to the start of each line in the file.}
  }
}
\section{Extends}{
The virtual class \code{"\linkS4class{ViewerData}"}, directly.
}
\section{Methods}{
  \describe{
    \item{colNames}{\code{signature(data = "ViewerDataText", cols =
        "ANY")}:
      provide the names of the specified columns. }
    \item{colWidths}{\code{signature(data = "ViewerDataText")}: 
      provide the number of characters in each of the specified columns.}
    \item{dimensions}{\code{signature(data = "ViewerDataText")}: 
      provide the number of rows and columns in the data.}
    \item{getText}{\code{signature(data = "ViewerDataText", rows = "ANY", cols = "ANY")}: 
      provide a text version of a subset of the data as specified by the
      rows and columns.}
  }
}
\author{Paul Murrell}
\seealso{
  \code{\linkS4class{Viewer}}
}
\examples{
showClass("ViewerDataText")
}
\keyword{classes}
