\name{ViewerDeviceDefault-class}
\Rdversion{1.1}
\docType{class}
\alias{ViewerDevice-class}
\alias{ViewerDeviceDefault-class}
\alias{drawData,ANY,ANY,ANY,ViewerDeviceDefault-method}
\alias{drawHead,ANY,ANY,ViewerDeviceDefault-method}
\alias{drawRowNames,ANY,ANY,ViewerDeviceDefault-method}
\alias{getFontForHeight,ViewerDeviceDefault-method}
\alias{getFontForWidth,ViewerDeviceDefault-method}
\alias{numChars,ViewerDeviceDefault-method}
\alias{numRows,ViewerDeviceDefault-method}

\title{Class "ViewerDeviceDefault" }
\description{
  An object representing the device that the data are being viewed on.
}
\section{Objects from the Class}{
  Objects can be created using the \code{viewerDeviceDefault} function.
}
\section{Extends}{
The virtual class \code{"\linkS4class{ViewerDevice}"}, directly.
}
\section{Methods}{
  \describe{
    \item{drawData}{\code{signature(x = "ANY", rows = "ANY", cols =
        "ANY", dev = "ViewerDeviceDefault")}:
      render the data.}
    \item{drawHead}{\code{signature(x = "ANY", cols = "ANY", dev =
        "ViewerDeviceDefault")}:
      render column names.}
    \item{drawRowNames}{\code{signature(x = "ANY", rows = "ANY", dev =
        "ViewerDeviceDefault")}:
      render row names.}
    \item{getFontForHeight}{\code{signature(dev =
        "ViewerDeviceDefault")}:
      determine an appropriate font size to show the specified number of
      rows.
    }
    \item{getFontForWidth}{\code{signature(dev =
        "ViewerDeviceDefault")}:
      determine an appropriate font size to show the specified number of
      columns.
    }
    \item{numChars}{\code{signature(dev = "ViewerDeviceDefault")}:
      determine how many characters can currently be seen on each row.
    }
    \item{numRows}{\code{signature(dev = "ViewerDeviceDefault")}:
      determine how many rows can currently be seen.
    }
  }
}
\author{Paul Murrell}
\seealso{
  \code{\linkS4class{Viewer}}
}
\examples{
showClass("ViewerDeviceDefault")
}
\keyword{classes}
