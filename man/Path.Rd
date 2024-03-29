% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Path.R
\name{Path}
\alias{Path}
\title{Path}
\usage{
Path(folder, test, type)
}
\arguments{
\item{folder}{Place of output files from ConQuest.}

\item{test}{Name of test.}

\item{type}{Type of output file from ConQuest such as 'txt', 'its', 'shw',
and 'itn'.}
}
\value{
Path of the file.
}
\description{
This function generates file path to a specified type. This is associated
with test named 'test'.
}
\examples{
file_path_type(test='FPA', type='its')
}
