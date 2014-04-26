\name{dirstat}
\alias{dirstat}
\alias{dirstat.default}
\alias{dirstat.webhdfs}
\title{dirstat: Status of a Directory on a FileSystem}
\usage{
dirstat(fs, path, ...)

\method{dirstat}{default}(fs, path, ...)

\method{dirstat}{webhdfs}(fs, path, ...)
}
\arguments{
  \item{fs}{FileSystem object}

  \item{path}{a character vector that contains the path of
  directory}

  \item{...}{other arguments}
}
\value{
a list of status information on directory
}
\description{
Status of a Directory on a FileSystem
}

