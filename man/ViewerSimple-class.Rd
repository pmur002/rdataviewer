\name{ViewerSimple-class}
\Rdversion{1.1}
\docType{class}
\alias{Viewer-class}
\alias{ViewerSimple-class}
\alias{draw,ViewerSimple-method}
\alias{lrend,ViewerSimple-method}
\alias{lrgrow,ViewerSimple-method}
\alias{lrhome,ViewerSimple-method}
\alias{lrscroll,ViewerSimple-method}
\alias{lrshrink,ViewerSimple-method}
\alias{udend,ViewerSimple-method}
\alias{udgrow,ViewerSimple-method}
\alias{udhome,ViewerSimple-method}
\alias{udscroll,ViewerSimple-method}
\alias{udshrink,ViewerSimple-method}

\title{Class "ViewerSimple" }
\description{An object representing a view of a data set.}
\section{Objects from the Class}{
  Objects can be created using the \code{simpleViewer} function.
}
\section{Slots}{
  \describe{
    \item{\code{dev}:}{Object of class \code{"ViewerDevice"}, typically
      the result of a call to \code{viewerDeviceVp}.}
    \item{\code{state}:}{Object of class \code{"ViewerState"},
      typically the result of a call to \code{viewerState}.}
    \item{\code{data}:}{Object of class \code{"ViewerData"}, typically
      the result of a call to the function \code{viewerData} or
      \code{viewerDataFrame}.}
    \item{\code{startcol}:}{Object of class \code{"numeric"} giving
      the first column in the current view.}
    \item{\code{startrow}:}{Object of class \code{"numeric"} giving
      the first row in the current view. }
    \item{\code{threshold}:}{Object of class \code{"numeric"}
      giving the threshold below which lines are drawn rather than text.
    }
  }
}
\section{Extends}{
The virtual class \code{"\linkS4class{Viewer}"}, directly.
}
\section{Methods}{
  \describe{
    \item{draw}{\code{signature(v = "ViewerSimple")}: render the
      current view of the data.}
    \item{lrend}{\code{signature(v = "ViewerSimple")}:
      navigate to the last column.}
    \item{lrgrow}{\code{signature(v = "ViewerSimple")}:
      zoom in the view to exclude the left/rightmost column.}
    \item{lrhome}{\code{signature(v = "ViewerSimple")}:
      navigate to the first column. }
    \item{lrscroll}{\code{signature(v = "ViewerSimple")}:
      navigate to the next/previous column. }
    \item{lrshrink}{\code{signature(v = "ViewerSimple")}:
      zoom out the view to include the next/previous column.
    }
    \item{udend}{\code{signature(v = "ViewerSimple")}:
      navigate to the last row.}
    \item{udgrow}{\code{signature(v = "ViewerSimple")}:
      zoom in the view to exclude first/last row.}
    \item{udhome}{\code{signature(v = "ViewerSimple")}:
      navigate to the first row.}
    \item{udscroll}{\code{signature(v = "ViewerSimple")}:
      navigate to the next/previous row.}
    \item{udshrink}{\code{signature(v = "ViewerSimple")}:
      zoom out the view to include the next/previous row.}
  }
}
\author{Paul Murrell}
\examples{
showClass("ViewerSimple")
}
\keyword{classes}
