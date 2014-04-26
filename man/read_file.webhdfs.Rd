\name{read_file.webhdfs}
\alias{read_file.webhdfs}
\title{Read to a File on HDFS}
\usage{
\method{read_file}{webhdfs}(fs, path, offset = NULL, length = NULL,
  buffersize = NULL, ...)
}
\arguments{
  \item{fs}{HDFS FileSystem object}

  \item{path}{a character vector that contains the path of
  file to read}

  \item{offset}{The starting byte position}

  \item{length}{The number of bytes to be processed}

  \item{buffersize}{used in transferring data}

  \item{...}{additional arguments passed to
  \code{\link[RCurl]{getURL}}}
}
\value{
a character vector that contains the contents of the File
}
\description{
Read to a File on HDFS
}
\references{
\href{http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html}{WebHDFS}
\href{http://hadoop.apache.org/docs/stable/api/org/apache/hadoop/fs/FileSystem.html}{HDFSFileSystem}
}

