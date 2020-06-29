\name{confusionMatrix_metrics}
\alias{confusionMatrix_metrics}
\title{
Get values derived from a confusion matrix 
}
\description{
Get values of a confusion matrix and some confusion-matrix-derived metrics (Note: this is a sub-function that shouldn't have to be used by package users) 
}
\usage{
confusionMatrix_metrics(cumSums, rankings = seq_along(cumSums), total = length(cumSums), seed = 102)
}
\arguments{
  \item{cumSums}{
Cumulative sums
}
  \item{rankings}{
Rankings of the Cumulative sums
}
  \item{total}{
Total numbers of cumulative sums
}
  \item{seed}{
The random seed number
}
}
\author{
Peter I-Fan Wu
}

