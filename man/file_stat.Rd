\name{file_stat}
\alias{file_stat}
\alias{file_stat.default}
\alias{file_stat.webhdfs}
\title{file_stat: Status of a File/Directory on a FileSystem}
\usage{
file_stat(fs, path, ...)

\method{file_stat}{default}(fs, path, ...)

\method{file_stat}{webhdfs}(fs, path, ...)
}
\arguments{
  \item{fs}{FileSystem object}

  \item{path}{a character vector that contains the path of
  file/directory}

  \item{...}{other arguments}
}
\value{
a list of status information on file/directory
}
\description{
Status of a File/Directory on a FileSystem
}

