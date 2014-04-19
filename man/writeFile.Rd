\name{writeFile}
\alias{writeFile}
\alias{writeFile.default}
\title{writeFile: Write to a File on a FileSystem}
\usage{
writeFile(fs, targetPath, ...)

\method{writeFile}{default}(fs, targetPath, ...)
}
\arguments{
  \item{fs}{FileSystem object}

  \item{targetPath}{a character vector that contains the
  path of file to write}

  \item{...}{other arguments}
}
\value{
\code{TRUE} if seccessful, \code{FALSE} otherwise
}
\description{
Write to a File on a FileSystem
}

