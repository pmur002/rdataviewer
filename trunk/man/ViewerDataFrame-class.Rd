\name{ViewerDataFrame-class}
\Rdversion{1.1}
\docType{class}
\alias{ViewerDataFrame-class}
\alias{colNames,ViewerDataFrame,numeric-method}
\alias{colWidths,ViewerDataFrame-method}
\alias{dimensions,ViewerDataFrame-method}
\alias{getText,ViewerDataFrame,numeric,numeric-method}
\alias{rowNames,ViewerDataFrame-method}
\alias{rowNameWidth,ViewerDataFrame-method}

\title{Class "ViewerDataFrame"}
\description{
  An object representing the data frame to view.
}
\section{Objects from the Class}{
  Objects can be created using the \code{viewerDataFrame} function.
}
\section{Slots}{
  \describe{
    \item{\code{df}:}{Object of class \code{"data.frame"} }
    \item{\code{widths}:}{Object of class \code{"numeric"} giving
      the number of characters in each column of a text version
      of the data frame.}
  }
}
\section{Extends}{
The virtual class \code{"\linkS4class{ViewerData}"}, directly.
}
\section{Methods}{
  \describe{
    \item{colNames}{\code{signature(data = "ViewerDataFrame", cols =
        "numeric")}:
      provide the names of the specified columns.
    }
    \item{colWidths}{\code{signature(data = "ViewerDataFrame")}:
      provide the number of characters in each of the specified columns.
    }
    \item{dimensions}{\code{signature(data = "ViewerDataFrame")}:
      provide the number of rows and columns in the data.
    }
    \item{getText}{\code{signature(data = "ViewerDataFrame", rows =
        "numeric", cols = "numeric")}:
      provide a text version of a subset of the data as specified by the
      rows and columns.
    }
    \item{rowNames}{\code{signature(data = "ViewerDataFrame")}:
      provide labels for the specified rows.
    }
    \item{rowNameWidth}{\code{signature(data = "ViewerDataFrame")}:
      provide the maximum width of the row names.
    }
  }
}
\author{Paul Murrell}
\seealso{
  \code{\linkS4class{Viewer}}
}
\examples{
showClass("ViewerDataFrame")
}
\keyword{classes}
