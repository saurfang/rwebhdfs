\name{dir_stat}
\alias{dir_stat}
\alias{dir_stat.default}
\alias{dir_stat.webhdfs}
\title{dir_stat: Status of a Directory on a FileSystem}
\usage{
dir_stat(fs, path, ...)

\method{dir_stat}{default}(fs, path, ...)

\method{dir_stat}{webhdfs}(fs, path, ...)
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

