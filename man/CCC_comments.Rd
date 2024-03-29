% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CCC_comments.R
\name{CCC_comments}
\alias{CCC_comments}
\title{CCC_comments}
\usage{
CCC_comments(test, dFallThr = 0.5, dRiseThr = 0.1, ccc_data, iType)
}
\arguments{
\item{test}{Name of test.}

\item{dFallThr}{Proportion on last bin above which falling distractor can be flagged.
Default is 0.5.}

\item{dRiseThr}{Proportion on last bin below which rising distractor is unflagged.
Default is 0.1.}

\item{ccc_data}{Data to draw CCC. One element of list output from Function 'CCC_Vernon'.}

\item{iType}{Dataframe with columns of iNum and itype. One element of list
output from Function 'CCC_Vernon'.}
}
\value{
Vector of comments based solely on CCC.
}
\description{
For dichotomous items, examine distractor CCC and add comments if curve is
against theory.
For polytomous items, add in comments from Function 'itn_poly_comment'.
This is associated with test named 'test'.
}
\examples{
comments <- CCC_comments(test='racp', ccc_data=ccc_data, iType=iType)
}
