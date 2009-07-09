\name{rdataviewer-package}
\Rdversion{1.1}
\docType{package}
\alias{rdataviewer-package}
\alias{rdataviewer}
\title{
  The rdataviewer package.
}
\description{
  There are four steps involved in viewing an R object with
  this package:

  \enumerate{
    \item
      Create an object of class \code{ViewerData}, which represents
      the object to be viewed.  The \code{viewerData()} function
      can be used for simple atomic vectors.  The
      \code{viewerDataFrame()} function is for data frames
      and the \code{viewerDataText()} is for external text files.
    \item
      Create an object of class \code{ViewerDevice}, which represents
      the graphics device that the object is going to be viewed on.
      The \code{viewerDevice()} function creates an onscreen device.
      The \code{viewerDataVp()} function creates an device with
      room around the edges for row names and column names.
    \item
      Create an object of class \code{ViewerState}, which represents
      the current view of the data, i.e., parameters that specify
      how the data are currently being viewed.
      The \code{viewerState()} function generates an initial state.
    \item
      Create an object of class \code{Viewer}, which contains
      the data to be viewed, the device it is being viewed on,
      and information about the current view, i.e.,
      instances of the previous three classes.
      The \code{simpleViewer()} function performs this step.
  }

  The view of the data can then be rendered by calling the
  \code{draw()} function on the \code{Viewer}.

  The view can be altered by calling one of the functions that
  manipulate the view, such as \code{lrscroll()} or
  \code{udscroll()}.  These return the modified view, which can
  then be rendered by calling \code{draw()} again.

  An interactive interface, based on tcltk, is also provided
  to allow rapid modification and automatic redrawing of a view.
  This is provided by the \code{tcltkViewer()} function.
}
\seealso{
\code{\link{viewerData}}, 
\code{\link{viewerDevice}},
\code{\link{viewerState}},
\code{\link{simpleViewer}}, 
\code{\link{tcltkViewer}}.
}
\author{
  Paul Murrell
}
