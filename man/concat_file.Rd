\name{concat_file}
\alias{concat_file}
\alias{concat_file.default}
\alias{concat_file.webhdfs}
\title{concat_file: Concat File(s) on a FileSystem}
\usage{
concat_file(fs, targetPath, ...)

\method{concat_file}{default}(fs, targetPath, sourcePath, ...)

\method{concat_file}{webhdfs}(fs, targetPath, sourcePath, ...)
}
\arguments{
  \item{fs}{FileSystem object}

  \item{targetPath}{a character vector that contains the
  path of file to write}

  \item{sourcePath}{a list of character vector that
  contains the path of file(s) to read}

  \item{...}{other arguments}
}
\value{
\code{TRUE} if successful, \code{FALSE} otherwise
}
\description{
Concat File(s) on a FileSystem
}

