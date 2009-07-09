\name{ViewerStateSimple-class}
\Rdversion{1.1}
\docType{class}
\alias{ViewerState-class}
\alias{ViewerStateSimple-class}
\alias{fontsize<-,ViewerStateSimple-method}
\alias{fontsize,ViewerStateSimple-method}
\alias{lrmode<-,ViewerStateSimple-method}
\alias{lrmode,ViewerStateSimple-method}
\alias{udmode<-,ViewerStateSimple-method}
\alias{udmode,ViewerStateSimple-method}

\title{Class "ViewerStateSimple"}
\description{
  A class representing how a data source is being viewed.
}
\section{Objects from the Class}{
  Objects can be created using the \code{viewerState} function.
  The default horizontal mode is \code{"left-to-right"},
  the default vertical mode is \code{"top-to-bottom"},
  and the default font size is \code{10}.
}
\section{Slots}{
  \describe{
    \item{\code{lrmode}:}{Object of class \code{"character"} with value
      \code{"left-to-right"} or \code{"right-=to-left"}.  Describes
      the current horizontal viewing mode.}
    \item{\code{udmode}:}{Object of class \code{"character"} with value
      \code{"top-to-bottom"} or \code{"bottom-to-top"}.
      Describes the current vertical viewing mode.}
    \item{\code{fontsize}:}{Object of class \code{"numeric"} that
      describes the current font size used to view the data.}
  }
}
\section{Extends}{
  Virtual class \code{"\linkS4class{ViewerState}"}, directly.
}
\section{Methods}{
  \describe{
    \item{fontsize<-}{\code{signature(state = "ViewerStateSimple")}:
      sets the font size.
    }
    \item{fontsize}{\code{signature(state = "ViewerStateSimple")}: gets
      the fontsize.
    }
    \item{lrmode<-}{\code{signature(state = "ViewerStateSimple")}: sets
      the horizontal mode.
    }
    \item{lrmode}{\code{signature(state = "ViewerStateSimple")}: gets
      the horizontal mode.
    }
    \item{udmode<-}{\code{signature(state = "ViewerStateSimple")}: sets
      the vertical mode.
    }
    \item{udmode}{\code{signature(state = "ViewerStateSimple")}: gets
      the vertical mode.
    }
  }
}
\author{Paul Murrell}
\seealso{
  \code{\linkS4class{Viewer}}.
}
\examples{
showClass("ViewerStateSimple")
}
\keyword{classes}