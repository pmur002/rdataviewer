\name{ViewerDeviceViewport-class}
\Rdversion{1.1}
\docType{class}
\alias{ViewerDeviceViewport-class}
\alias{drawData,ANY,ANY,ANY,ViewerDeviceViewport-method}
\alias{drawHead,ANY,ANY,ViewerDeviceViewport-method}
\alias{drawRowNames,ANY,ANY,ViewerDeviceViewport-method}
\alias{getFontForHeight,ViewerDeviceViewport-method}
\alias{getFontForWidth,ViewerDeviceViewport-method}
\alias{numChars,ViewerDeviceViewport-method}
\alias{numRows,ViewerDeviceViewport-method}

\title{Class "ViewerDeviceViewport" ~~~ }
\description{
  An object representing the device that the data are being viewed on.
}
\section{Objects from the Class}{
  Objects can be created using the \code{viewerDeviceVp} function.
}
\section{Slots}{
  \describe{
    \item{\code{datavp}:}{Object of class \code{"viewport"} giving
      the viewport in which to draw the data.}
    \item{\code{headvp}:}{Object of class \code{"viewport"} giving
      the viewport in which to draw the column headers.}
    \item{\code{rownamevp}:}{Object of class \code{"viewport"} giving
      the viewport in which to draw the row labels. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{ViewerDeviceDefault}"}, directly.
The virtual class \code{"\linkS4class{ViewerDevice}"}, by class "ViewerDeviceDefault", distance 2.
}
\section{Methods}{
  \describe{
    \item{drawData}{\code{signature(x = "ANY", rows = "ANY", cols =
        "ANY", dev = "ViewerDeviceViewport")}:
      render the data.
    }
    \item{drawHead}{\code{signature(x = "ANY", cols = "ANY", dev =
        "ViewerDeviceViewport")}:
      render column names.
    }
    \item{drawRowNames}{\code{signature(x = "ANY", rows = "ANY", dev =
        "ViewerDeviceViewport")}:
      render row names.
    }
    \item{getFontForHeight}{\code{signature(dev =
        "ViewerDeviceViewport")}:
      determine an appropriate font size to show the specified number of
      rows.
    }
    \item{getFontForWidth}{\code{signature(dev =
        "ViewerDeviceViewport")}:
      determine an appropriate font size to show the specified number of
      columns.
    }
    \item{numChars}{\code{signature(dev = "ViewerDeviceViewport")}:
      determine how many characters can currently be seen on each row.
    }
    \item{numRows}{\code{signature(dev = "ViewerDeviceViewport")}:
      determine how many rows can currently be seen.
    }
  }
}
\author{Paul Murrell}
\seealso{
  \code{\linkS4class{Viewer}}
}
\examples{
showClass("ViewerDeviceViewport")
}
\keyword{classes}
