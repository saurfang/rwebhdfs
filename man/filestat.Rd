\name{filestat}
\alias{filestat}
\alias{filestat.default}
\alias{filestat.webhdfs}
\title{filestat: Status of a File/Directory on a FileSystem}
\usage{
filestat(fs, path, ...)

\method{filestat}{default}(fs, path, ...)

\method{filestat}{webhdfs}(fs, path, ...)
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

