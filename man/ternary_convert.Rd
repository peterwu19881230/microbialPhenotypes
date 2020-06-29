\name{ternary_convert}
\alias{ternary_convert}
\title{
Discretize data to ternary form
\description{
Convert values within a matrix to binary form (-1, 0 or 1)
}
\usage{
ternary_convert(matrix, threshold)
}
\arguments{
  \item{matrix}{
  A matrix that contains numbers in real values
}
  \item{threshold}{
  A positive real value that determines the cutoff. |Value|<cutoff will be converted to 0. Value > cutoff will be converted to 1. Value < -cutoff will be converted to -1
}
}
\author{
Peter I-Fan Wu
}
\examples{
mat=matrix(c(-6:5),nrow=3)
ternary_convert(mat,threshold = 3)
}
