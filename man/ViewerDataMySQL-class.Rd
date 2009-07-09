\name{ViewerDataMySQL-class}
\Rdversion{1.1}
\docType{class}
\alias{ViewerDataMySQL-class}
\alias{colNames,ViewerDataMySQL,ANY-method}
\alias{colWidths,ViewerDataMySQL-method}
\alias{dimensions,ViewerDataMySQL-method}
\alias{getText,ViewerDataMySQL,ANY,ANY-method}

\title{Class "ViewerDataMySQL" }
\description{
  An object representing the SQL query to view.
}
\section{Objects from the Class}{
  Objects can be created using the \code{viewerDataMySQL} function.
}
\section{Slots}{
  \describe{
    \item{\code{dbcon}:}{Object of class \code{"MySQLConnection"}
      giving a connection to the database server.}
    \item{\code{query}:}{Object of class \code{"character"}
      giving the SQL query.}
    \item{\code{colnames}:}{Object of class \code{"character"}
      giving the column names of the query result.}
    \item{\code{paddednames}:}{Object of class \code{"character"}
      giving the column names padded to the maximum width of
      the columns. }
    \item{\code{widths}:}{Object of class \code{"numeric"}
      giving the number of characters in each column of the result.}
    \item{\code{nlines}:}{Object of class \code{"numeric"}
      giving the number of rows in the result. }
  }
}
\section{Extends}{
The virtual class \code{"\linkS4class{ViewerData}"}, directly.
}
\section{Methods}{
  \describe{
    \item{colNames}{\code{signature(data = "ViewerDataMySQL", cols =
        "ANY")}:
      provide the names of the specified columns.
    }
    \item{colWidths}{\code{signature(data = "ViewerDataMySQL")}: 
      provide the number of characters in each of the specified columns.
    }
    \item{dimensions}{\code{signature(data = "ViewerDataMySQL")}: 
      provide the number of rows and columns in the data.
    }
    \item{getText}{\code{signature(data = "ViewerDataMySQL", rows = "ANY", cols = "ANY")}: 
      provide a text version of a subset of the data as specified by the
      rows and columns. }
  }
}
\author{Paul Murrell}
\seealso{
  \code{\linkS4class{Viewer}}
}
\examples{
showClass("ViewerDataMySQL")
}
\keyword{classes}
