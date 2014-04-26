\name{get_webhdfs_home}
\alias{get_webhdfs_home}
\title{Get Home Directory}
\usage{
get_webhdfs_home(webhdfs, doas = NULL, ...)
}
\arguments{
  \item{webhdfs}{a \code{\link{webhdfs}} object}

  \item{doas}{user whose home folder will be retrieve}

  \item{...}{additional arguments passed to
  \code{\link[RCurl]{getURL}}}
}
\description{
Return the current user's home directory in this
filesystem. The default implementation returns
"/user/$USER/".
}

