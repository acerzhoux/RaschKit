% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sparse_data_process.R
\name{sparse_data_process}
\alias{sparse_data_process}
\title{sparse_data_process}
\usage{
sparse_data_process(
  test,
  respDf,
  keyDf,
  labels,
  n_cov,
  nDimVec,
  miss_code = c(".", "r", "R", "x", "X", "", " "),
  DIFVar = NULL
)
}
\arguments{
\item{test}{Name of test.}

\item{respDf}{Data to be processed.}

\item{keyDf}{Keys to the test items.}

\item{labels}{Item labels.}

\item{n_cov}{Number of covariables before reponses in the dataframe.}

\item{nDimVec}{Vector of number of items for each dimension in the test.}

\item{miss_code}{Code for missing data that should be coded to NA.}

\item{DIFVar}{Name of DIF variable. Should be lowercase for ConQuest to run.
Default is NULL.}
}
\value{
List of processed data, updated dimension test numbers, updated keys,
and updated labels.
}
\description{
This function preprocesses data in three ways. First, convert non-used
categories and missing values to NA. Second, remove item responses with
all missing values. Third, for DIF analysis, remove items that have no data
on any category of the DIF variable. This is associated with test named 'test'.
An Excel file with processed data and removed items will be saved to 'data'
folder in working directory.
}
\examples{
processed <- sparse_data_process(test='Dan', data=df_DIF, keyDf=rep(1,1072),
labels=rep(1,1072), n_cov=5, nDimVec=c(901, 171),
miss_code=c('.', 'r', 'R', 'x', 'X', '', ' '),
DIFVar='studGender')
}
