\name{curlWebHDFS}
\alias{curlWebHDFS}
\title{Dispatch a WebHDFS Curl request}
\usage{
curlWebHDFS(webhdfs, url, requestType = c("GET", "POST", "PUT"),
  doas = NULL, putContent = NULL, .opts = curlOptions(),
  headerfunction = NULL, ...)
}
\arguments{
  \item{webhdfs}{A \code{\link{webhdfs}} object}

  \item{url}{The curl to request}

  \item{requestType}{a character vector chosen from "GET",
  "POST", "PUT"}

  \item{doas}{proxy username}

  \item{putContent}{content to send in a PUT request}

  \item{.opts}{a \code{\link[RCurl]{curlOptions}} object}

  \item{headerfunction}{a function that processes the
  header information}

  \item{...}{additional arguments passed to
  \code{\link[RCurl]{getURL}}}
}
\value{
response content
}
\description{
Dispatch a WebHDFS Curl request
}

