\name{viewerDataMySQL}
\Rdversion{1.1}
\alias{viewerDataMySQL}
\title{
  Prepare an SQL query for viewing.
}
\description{
  This function derives various important attributes of an
  SQL query and stores them so that viewing the query
  can occur efficiently.
}
\usage{
viewerDataMySQL(query, dbname, username, password = "", host = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{query}{
    The SQL query.
  }
  \item{dbname}{
    The name of the database to be queried.
  }
  \item{username}{
    The username to connect to the database server.
  }
  \item{password}{
    The username password to connect to the database server.
  }
  \item{host}{
    The name of the database server.
  }
}
\details{
  This set up involves a couple of queries to the database server.
}
\value{
  An object of class \code{"\linkS4class{ViewerDataMySQL}"}.
}
\author{Paul Murrell}
\seealso{
  \code{\linkS4class{ViewerDataMySQL}}
  and
  \code{\link{viewerData}}
}
\examples{
}
\keyword{ dynamic }
