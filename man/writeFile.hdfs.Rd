\name{writeFile.webhdfs}
\alias{writeFile.webhdfs}
\title{Create and Write to a File on HDFS}
\usage{
\method{writeFile}{webhdfs}(fs, targetPath, srcPath, sizeWarn = 1e+08,
  append = FALSE, overwrite = FALSE, blocksize = NULL,
  replication = NULL, permission = 755, buffersize = NULL, ...)
}
\arguments{
  \item{fs}{HDFS FileSystem object}

  \item{targetPath}{a character vector that contains the
  path of file to write}

  \item{srcPath}{path of the file whose content will be
  written}

  \item{sizeWarn}{raise warning if file size exceeds
  threshold. Use \code{NULL} to suppress warning}

  \item{append}{to a file if \code{TRUE}}

  \item{overwrite}{a file if \code{TRUE} and file already
  exists}

  \item{blocksize}{hdfs blocksize for the file}

  \item{replication}{factor for the file}

  \item{permission}{of the file as in octal mask}

  \item{buffersize}{used in transferring data}

  \item{...}{additional arguments passed to
  \code{\link[RCurl]{getURL}}}
}
\value{
\code{TRUE} if successful, \code{FALSE} otherwise
}
\description{
Create and Write to a File on HDFS
}
\references{
\href{http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html#Create_and_Write_to_a_File}{WebHDFS}
\href{http://hadoop.apache.org/docs/stable/api/org/apache/hadoop/fs/FileSystem.html}{HDFS
FileSystem}
}

