% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/section_data.R
\name{section_data}
\alias{section_data}
\title{section_data}
\usage{
section_data(
  path_df,
  resps_cols,
  pid_cols,
  run_ls,
  regr_ls,
  path_lab,
  DIFVar,
  DIFVar_cols,
  poly_group,
  pweight,
  pw_cols
)
}
\arguments{
\item{path_df}{Path of data to import in 'data' folder.}

\item{resps_cols}{Column numbers of responses.}

\item{pid_cols}{Column numbers of person ID.}

\item{run_ls}{List of data filters. Element is column number in data.
Element name is filter variable's name.}

\item{regr_ls}{List of regressors. Element is column number in data.
Element name is regressor name.}

\item{path_lab}{Route of label file in 'input' folder.}

\item{DIFVar}{Name of DIF variable.}

\item{DIFVar_cols}{DIF variable's column number in data.}

\item{poly_group}{TRUE if model is run per group.}

\item{pweight}{Variable name of person weights in response dataframe. Should
be specified if weight is used for modeling.}

\item{pw_cols}{String of column numbers of case weight, e.g., '5-15'.}
}
\value{
String of characters used in export section of 'test.cqc' file in
'input' folder.
}
\description{
This function creates string of data specification for 'test.cqc' file in
'input' folder. This is associated with test named 'test'.
}
\details{
Variable names and variable column numbers are added when related arguments
are not NULL. Those addable variables are pid_cols (person ID), DIFVar
(DIF variable), run_ls (data filters), and regr_ls (regressors). Also,
models can be run per group if poly_group is TRUE.
}
\examples{
section_data()
}
