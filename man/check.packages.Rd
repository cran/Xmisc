% Generated by roxygen2 (4.0.1): do not edit by hand
\name{check.packages}
\alias{check.packages}
\title{Check if a package can be loaded}
\usage{
check.packages(x, envir = sys.frame(sys.parent(0)), character.only = FALSE)
}
\arguments{
\item{x}{package, see \code{library} or \code{require}.}

\item{envir}{the \code{environment} to use.}

\item{character.only}{see \code{library} or \code{require}.}
}
\value{
logical, whether a package can be loaded.
}
\description{
Check if a package can be loaded. If TRUE, load it as long as it has not yet been loaded.
}
\examples{
check.packages("Xmisc")
check.packages(Xmisc)
x <- "Xmisc"
check.packages(x,character.only=TRUE)
}
\author{
Xiaobei Zhao
}
\seealso{
\code{\link{is.package.loaded}}
}

