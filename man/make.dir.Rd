% Generated by roxygen2 (4.0.1): do not edit by hand
\name{make.dir}
\alias{make.dir}
\title{Make a directory recursively}
\usage{
make.dir(x, mode)
}
\arguments{
\item{x}{character, a directory name.}

\item{mode}{the mode of the path, see \code{dir.create}}
}
\description{
Make a directory recursively
}
\examples{
\dontrun{
if (character_to_logical(
  raw_input("Would you like to create a directory for testing
  at current working directory?",c('yes','no')))){
  ## make.dir('testdir','751') # uncomment it to let R create the directory
}
}
}
\author{
Xiaobei Zhao
}

