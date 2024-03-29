% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rlab_iDIFv.R
\name{rlab_iDIFv}
\alias{rlab_iDIFv}
\title{rlab_iDIFv}
\usage{
rlab_iDIFv(test, pid, grade, data_ls, lab_ls, cd_ls, DIF_ls, n_cov)
}
\arguments{
\item{test}{Name of test.}

\item{pid}{Variable name of person ID.}

\item{grade}{Variable name of students' grade.}

\item{data_ls}{List of dataframe for each grade. Name is grade, e.g., `2`.
Each dataframe has two parts, covariates (include `StudentID`, `grade`)
and responses.}

\item{lab_ls}{List of lables. Name is grade, e.g., `2`. Element is vector
of item labels (char).}

\item{cd_ls}{List of codebook dataframes. Name is grade. Element is dataframe
with variables of `Item ID`, `Key`. Make sure item order is same as item
response order in data_ls.}

\item{DIF_ls}{List of DIF item labels (char). Name is grade. Element is
DIF item labels.}

\item{n_cov}{Number of covariates before responses.}
}
\value{
List of column numbers of pid, responses, and grade regressors.
}
\description{
This function relabel DIF items that show up from vertical equating.
}
\details{
This function uses vertical equating results (DIF_ls) to relabel each
grade's item labels (lab_ls) and merge all grades' data into one. Second,
it adds a dummy regressor for each grade except the lowest grade. Third,
it saves merged data, revised labels, and keys into 'Data' folder. Finally,
it returns the column numbers of pid, responses, and grade regressors to
be used for 'test.cqc' file in 'input' folder.
}
\examples{
rlab_iDIFv()
}
