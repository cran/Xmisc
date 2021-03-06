% Generated by roxygen2 (4.0.1): do not edit by hand
\name{is.package.loaded}
\alias{is.package.loaded}
\title{Check if a package is loaded}
\usage{
is.package.loaded(x, envir = sys.frame(sys.parent(0)),
  character.only = FALSE)
}
\arguments{
\item{x}{package, see \code{library} or \code{require}.}

\item{envir}{the \code{environment} to use.}

\item{character.only}{see \code{library} or \code{require}.}
}
\value{
logical
}
\description{
Check if a package is loaded
}
\examples{
is.package.loaded(Xmisc)
is.package.loaded("Xmisc")
x <- "Xmisc"
is.package.loaded(x) #FALSE
is.package.loaded(x,character.only=TRUE) #TRUE
}
\author{
Xiaobei Zhao
}
\seealso{
\code{\link{check.packages}}
}

