\name{mkdir}
\alias{mkdir}
\alias{mkdir.default}
\alias{mkdir.webhdfs}
\title{mkdir: Create Directory Function}
\usage{
mkdir(fs, path, ...)

\method{mkdir}{default}(fs, path, ...)

\method{mkdir}{webhdfs}(fs, path, ...)
}
\arguments{
  \item{fs}{FileSystem object}

  \item{path}{a character vector that contains the path of
  folder to create}

  \item{...}{other arguments}
}
\value{
\code{TRUE} if seccessful, \code{FALSE} otherwise
}
\description{
Create directory on a FileSystem
}

