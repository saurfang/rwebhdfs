\name{rename_file}
\alias{rename_file}
\alias{rename_file.default}
\alias{rename_file.webhdfs}
\title{rename_file: Rename a File/Directory on a FileSystem}
\usage{
rename_file(fs, fromPath, toPath, ...)

\method{rename_file}{default}(fs, fromPath, toPath, ...)

\method{rename_file}{webhdfs}(fs, fromPath, toPath, ...)
}
\arguments{
  \item{fs}{FileSystem object}

  \item{fromPath}{a character vector that contains the path
  renaming from}

  \item{toPath}{a character vector that contains the path
  renaming to}

  \item{...}{other arguments}
}
\value{
\code{TRUE} if successful, \code{FALSE} otherwise
}
\description{
Rename a File/Directory on a FileSystem
}

