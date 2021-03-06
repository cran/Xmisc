% Generated by roxygen2 (4.0.1): do not edit by hand
\name{get_extdata}
\alias{get_extdata}
\title{Get the extdata file path of a package}
\usage{
get_extdata(pkg, name, dir = "extdata", mustWork = TRUE)
}
\arguments{
\item{pkg}{character, the name of a package}

\item{name}{character, the name of the extdata file}

\item{dir}{character, the directory in the package hierarchy}

\item{mustWork}{See \code{system.file}}
}
\value{
character, the extdata file path
}
\description{
Get the extdata file path of a package
}
\examples{
\dontrun{
try(get_extdata('datasets','morley.tab','data'))
}
}
\author{
Xiaobei Zhao
}

