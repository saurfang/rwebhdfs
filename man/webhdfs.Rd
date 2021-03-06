% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/webhdfs.R
\name{webhdfs}
\alias{webhdfs}
\title{Initialize WebHDFS Object}
\usage{
webhdfs(namenode_host, namenode_port, hdfs_username, securityON = FALSE,
  token = NULL)
}
\arguments{
\item{namenode_host}{namenode host address}

\item{namenode_port}{namenode host port}

\item{hdfs_username}{webhdfs access username}

\item{securityON}{\code{TRUE} if HDFS security is on, \code{FALSE} otherwise}

\item{token}{Hadoop delegation token to use when security is ON. \code{NULL} implies Kerberos SPNEGO authentication if \code{securityON} is \code{TRUE}}
}
\value{
a webhdfs object that captures the configuration
}
\description{
Initialize WebHDFS Object
}
\section{Enable WebHDFS}{

To enable WebHDFS in your Hadoop Installation add the following configuration 
to your hdfs_site.xml (requires Hadoop >0.20.205.0):

 <property>
 <name>dfs.webhdfs.enabled</name>
 <value>true</value>
 </property>  
 
 see: \url{https://issues.apache.org/jira/secure/attachment/12500090/WebHdfsAPI20111020.pdf}
}

