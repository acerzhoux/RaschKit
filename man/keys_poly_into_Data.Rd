% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/keys_poly_into_Data.R
\name{keys_poly_into_Data}
\alias{keys_poly_into_Data}
\title{keys_poly_into_Data}
\usage{
keys_poly_into_Data(test, score_max, N_item)
}
\arguments{
\item{test}{Name of test.}

\item{score_max}{Maximum score of single polytomous items in the test.}

\item{N_item}{Number of items in the test.}
}
\description{
This function saves raw key file 'test_Key.txt' into 'data' folder. Later,
this saved file is used to produce the key section of 'test.cqc' file in
'input' folder. This is associated with polytomous item test named 'test'.
}
\examples{
keys_poly_into_Data()
}
