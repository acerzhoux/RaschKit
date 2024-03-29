% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/response_repeat.R
\name{response_repeat}
\alias{response_repeat}
\title{response_repeat}
\usage{
response_repeat(df, begin, end, codes, rep_min)
}
\arguments{
\item{df}{Dataframe that contains responses.}

\item{begin}{The 1st item's order in test, e.g., 7.}

\item{end}{The last item order in test, e.g., 46.}

\item{codes}{Option(s) to check for repetition, e.g., c('A','C')}

\item{rep_min}{Minimum number of repetition, e.g., 8.}
}
\value{
Subset of input responses with a minimum number of repetitions of option(s).
}
\description{
This function extracts candidates who have a minimum number of repetitions of option(s).
}
\examples{
a <- response_repeat(df=elena[10:48], begin=1, end=39)
}
