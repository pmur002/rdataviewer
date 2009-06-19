\name{tcltkViewer}
\Rdversion{1.1}
\alias{tcltkViewer}
\title{
  A tcltk data viewer
}
\description{
  Displays a data source in a graphics window and creates
  a dialog to allow navigation of the data display via key strokes.
}
\usage{
tcltkViewer(v, bg = "grey90", region = "red")
}
\arguments{
  \item{v}{
    An object of class \code{Viewer}.
  }
  \item{bg}{
    The colour used for the background of the data overview
    (see Details below).
  }
  \item{region}{
    The colour used for the viewed region in the data overview
    (see Details below).
  }
}
\details{
  The dialog contains key-stroke instructions for navigation,
  plus an overview of the data source being viewed.  This
  overview consists of a rectangle with the approximate dimensions
  of the entire data source and a smaller rectangle (red by default)
  that shows what portion of the data source is currently
  being viewed in the graphics window.

  The dialog must have focus for the key-strokes to have any effect
  on the view in the graphics window.
}
\author{
  Paul Murrell
}
\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
}
\keyword{ dynamic }
