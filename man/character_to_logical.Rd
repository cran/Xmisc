% Generated by roxygen2 (4.0.1): do not edit by hand
\name{character_to_logical}
\alias{character_to_logical}
\title{Convert a character string to logical.}
\usage{
character_to_logical(x, ignore.case = TRUE)
}
\arguments{
\item{x}{character}

\item{ignore.case}{logical, whether case is ignored}
}
\value{
logical. TRUE for "y","yes","t","true" and "1";
FALSE for "n","no","f","false" and "0".
}
\description{
Convert a character string to logical.
}
\examples{
character_to_logical("yes")
try(character_to_logical("hi"))
}
\author{
Xiaobei Zhao
}

