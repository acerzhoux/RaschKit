% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DIF_dich_its_shw.R
\name{DIF_dich_its_shw}
\alias{DIF_dich_its_shw}
\title{DIF_dich_its_shw}
\usage{
DIF_dich_its_shw(
  DIFVar,
  test,
  vars,
  p_cut = 0.05,
  DIF_cut = 0.5,
  DIF_std_cut = 4,
  design_effect = 1,
  step = FALSE,
  save_xlsx = TRUE,
  iter = FALSE,
  quick = TRUE
)
}
\arguments{
\item{DIFVar}{Name of dichotomous DIF variable, e.g., 'gender'.}

\item{test}{Name of test.}

\item{vars}{Vector of DIF categories, e.g., c('Girls','Boys'). Order should
correspond to alphabetic/numeric order of DIF variables' two categories' code in data.}

\item{p_cut}{p value of chi-square test. Default is 0.05.}

\item{DIF_cut}{Threshold of an item's delta estimate difference between two
tests. Default is 0.5.}

\item{DIF_std_cut}{Threshold of an item's standardized delta estimate difference
between two tests. Default is 4.}

\item{design_effect}{Value to adjust errors. Default is 1.}

\item{step}{TRUE if DIF analysis is performed on step parameters. Default is FALSE.}

\item{save_xlsx}{Whether to save summary file and plots. Default is TRUE
(one DIF variable).}

\item{iter}{TRUE to iteratively remove DIF items. Default is FALSE}

\item{quick}{Whether quick error is needed. Default is TRUE for DIF analysis.}
}
\value{
List of summary of results from dichotomous DIF variable analysis,
including comments, step, summary statistics with flags, and statistics of
items after review.
}
\description{
This function performs chi-square tests (DIF analysis) on all items's
difference of delta estimates between two groups of test takers.
An Excel file with test results and flags is saved in 'DIF' folder.
Also, scatterplot of delta  before and after review is saved in
subfolder 'plot' inside 'DIF' folder.
}
\examples{
# Not run
# DIF_dich_its_shw(DIFVar='ATSI', test='WA_step', vars=c('ATSI_No','ATSI_Yes'))
}
