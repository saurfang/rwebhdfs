rwebhdfs
========
[![Build Status](https://travis-ci.org/saurfang/rwebhdfs.svg?branch=master)](https://travis-ci.org/saurfang/rwebhdfs)
R Package for WebHDFS REST API

## Overview
This R package provides access to HDFS via WebHDFS REST API. For more information, please see:
http://hadoop.apache.org/docs/stable/hadoop-project-dist/hadoop-hdfs/WebHDFS.html

## Hadoop Configuration
Ensure that WebHDFS is enabled in the `hdfs-site.xml`
```
<property>
    <name>dfs.webhdfs.enabled</name>
    <value>true</value>
 </property>
```

## How to Use
More examples will arrive in the function help pages but for now, here's a brief guide on how to use `rwebhdfs`

#### Environment
I'm recommend HDP 2.0 for quick demo and testing: http://hortonworks.com/hdp/downloads/

#### Create your webhdfs object
WebHDFS is a S3 object and can be created using 
```R
hdfs <- webhdfs("localhost", 50070, "hue")
```

#### List the files under you home directory
```R
dir_stat(hdfs, "")
```

#### Creates an empty file named "test" and get its information
```R
write_file(hdfs, "test")
file_stat(hdfs, "test")
```

#### Write local file onto HDFS and see what we just wrote
```R
foo <- tempfile()
writeLines("foobar", foo)
write_file(hdfs, "foo", foo)
read_file(hdfs, "foo")
```

#### Creates a directory and move our file in it
```R
mkdir(hdfs, "bar")
rename_file(hdfs, "foo", "bar/foo")
```

#### Finally delete the test file and folder
```R
delete_file(hdfs, "test")
delete_file(hdfs, "bar", recursive=TRUE)
```

## How to Install
rwebhdfs is not on CRAN yet. I plan to play with it in a couple Hadoop projects before submission to CRAN. So that I can decide if all functions are intuitive and well designed.

To get latest version on Github:
```R
devtools::install_github(c("saurfang/rwebhdfs"))
```

## Implementation
`webhdfs` has been implemented as a S3 object and all common FileSystem related functions are coded as S3 methods. Since R provides some basic FileSystem functions like `list.files`, `file.info`, `read.*`, `write.*` and etc, I try to name my functions in a similar logic but easy to find using auto-completion when actually typing. So you will find functions like `write_file`, `file_stat`, `rename_file` and etc.

It seems that in Hadoop itself, WebHDFS has been implemented as a subclass of FileSystem and there are a lot of others like FTP, S3 and (regular) HDFS that extend to this interface. I think it would be awesome if we do the same in R so data can be fetched/stored in a more transparent way from different FileSystem.

Discussion is more than welcomed on design decisions and choice on OO System. I have zero experience on OO programing in R and chose S3 based on the suggestions here: http://adv-r.had.co.nz/OO-essentials.html

## Authentication
Both Kerberos and delegation token security are implemented. Use the `securityON` flag in `webhdfs` constructor to enable security, if in addition `token` is also supplied then delegation token will be used, otherwise Kerberos is assumed. However, I have not tested this feature just yet. Please report any issues you see.
